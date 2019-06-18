package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConverters._
import org.eclipse.jdt.core.dom
import expression._
import statement._
import method._
import importdeclaration._
import com.typesafe.scalalogging.LazyLogging
import org.eclipse.jdt.core.dom.Modifier
import java.io.File
import org.eclipse.jdt.core.dom.SuperConstructorInvocation
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import java.nio.file.Path
import org.eclipse.jdt.core.dom.ITypeBinding
import org.eclipse.jdt.internal.compiler.ASTVisitor
import org.eclipse.jdt.core.dom.MethodDeclaration

object compilationunit extends LazyLogging {

  def isDeprecated(javadoc: dom.Javadoc): Boolean = {
    if (javadoc != null && javadoc.tags != null)
      javadoc.tags.asScala collect { case x: dom.TagElement => x } exists { x => x.getTagName == "@deprecated" }
    else false
  }

  /**
   * @param cu Java Eclipse AST CompilationUnit
   * @param root Path to source root folder
   * @param file Path to source file
   * @param name File name without extension
   * @return ESTree Program node
   */
  def fromCompilationUnit(cu: dom.CompilationUnit, root: Path, file: Path, name: String): Program = {
    val types = cu.types.asScala collect { case x: dom.TypeDeclaration if !isDeprecated(x.getJavadoc) => fromTypeDeclaration(x) } flatten

    if (types.length == 0) return null

    val classDeclaration = types.head.asInstanceOf[ClassDeclaration]
    val staticClassDeclarations = types.tail

    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    val tree = mapper.valueToTree[JsonNode](types)

    def compareIdentifier(node: JsonNode, name: String): Boolean = {
      if (node.get("type").asText() != "Identifier") return false
      var nodeName = node.get("name").asText()
      
      var parts = nodeName.split('.')
      if (parts.length > 1) 
        return parts(0) == name
      else
        return nodeName == name
    }
    
    def countIdentifier(name: String) =
      tree.findParents("type").asScala.filter(compareIdentifier(_, name)).size

    val packageImports = if (cu.getPackage != null)
      importsFromName(cu.getPackage.getName.getFullyQualifiedName, root, name)
    else
      Map[String, String]()

    val explicitImports = cu.imports.asScala collect { case x: dom.ImportDeclaration => fromImportDeclaration(x, root, file) }
    val distinctImports = (builtinImports(root, file) ++ packageImports ++ explicitImports) - classDeclaration.id.name
    //val distinctImports = (packageImports ++ explicitImports) - classDeclaration.id.name
    val usedImports = distinctImports.filter { case (name, path) => countIdentifier(name) > 0 }

    val imports = usedImports.collect { case (name, path) => createImport(name, path) }

    val exportedTypes = new ExportDefaultDeclaration(classDeclaration) +: staticClassDeclarations
    new Program("module", imports ++ exportedTypes)
  }

  def fromSingleVariableDeclaration(x: dom.SingleVariableDeclaration): Identifier =
    new Identifier(x.getName.getIdentifier)

  def fromParameters(parameters: java.util.List[_]) =
    parameters.asScala collect { case x: dom.SingleVariableDeclaration => fromSingleVariableDeclaration(x) }

  def fromBlock2(bs: dom.Block)(implicit td: dom.TypeDeclaration): Iterable[Statement] =
    bs.statements.asScala collect { case statement: dom.Statement => fromStatement(statement) }

  def fromBlock(bs: dom.Block)(implicit td: dom.TypeDeclaration): BlockStatement =
    if (bs == null)
      new BlockStatement(List())
    else
      new BlockStatement(
        bs.statements.asScala collect { case statement: dom.Statement => fromStatement(statement) })

  def createConstructor(constructors: Array[dom.MethodDeclaration], memberFields: Array[ExpressionStatement], hasSuper: Boolean)(implicit td: dom.TypeDeclaration) = {
    val statements = createConstructorBody(constructors, memberFields, hasSuper)
    val params = List()
    new FunctionExpression(
      params,
      new BlockStatement(statements),
      false)
  }

  def createConstructorBody(
      constructors: Array[dom.MethodDeclaration],
      memberFields: Array[ExpressionStatement],
      hasSuper: Boolean)(implicit td: dom.TypeDeclaration): Iterable[Statement] = {
    
    val statements = fromOverloadedMethodDeclarations(constructors, true)

    var hasExplicitSuperCall = hasSuper

    constructors.foreach {
      _.accept(new dom.ASTVisitor() {
        override def visit(node: SuperConstructorInvocation): Boolean = {
          hasExplicitSuperCall = false
          true
        }
      })
    }

    val defaultStatements = if (hasExplicitSuperCall) {
      // TODO: need to go into the correct constructor... 
      /*
      val apply = new MemberExpression("constructor_", "apply")
      var apply2 = new MemberExpression(new Super(), apply)
      val call = new CallExpression(apply2, List(new ThisExpression, new Identifier("arguments")))
      new ExpressionStatement(call) +: 
      */
      
      memberFields
    } else {
      memberFields
    }
    
    /*
    if (hasSuper) {
      var left = new MemberExpression(new Identifier("arguments"), new Identifier("length"))
      var right = new Literal(0, "0")
      var logicalExpression = new LogicalExpression("===", left, right) 
      var consequent = new ReturnStatement(null)
      var ifStatement = new IfStatement(logicalExpression, consequent, null)
      
      List(ifStatement) ++ defaultStatements ++ statements
    } else {
      defaultStatements ++ statements
    }
    */
    
    defaultStatements ++ statements
  }

  def createInterfacesProperty(interfaces: List[Identifier]): MethodDefinition = {
    val returnInterfaces = new ReturnStatement(new ArrayExpression(interfaces))
    new MethodDefinition(new Identifier("interfaces_"), new FunctionExpression(List(), new BlockStatement(List(returnInterfaces))), "get", false, false)
  }

  def fromTypeDeclaration(implicit td: dom.TypeDeclaration): List[Statement] = {
    val methods = td.getMethods filterNot { x => Modifier.isAbstract(x.getModifiers) || isDeprecated(x.getJavadoc) }
    val types = td.getTypes

    val constructors = methods filter { m => m.isConstructor() }
    val memberFields = td.getFields map { fromFieldDeclarationMember(_) } flatten
    val staticFields = td.getFields map { fromFieldDeclarationStatic(_) } flatten
    
    val hasSuperclass = td.getSuperclassType != null

    val constructor = createConstructor(constructors, memberFields, hasSuperclass)
    
    val left = new MemberExpression(td.getName.getIdentifier, new Identifier("constructor_"))
    val staticConstructor = new AssignmentExpression("=", left, constructor)

    val memberMethods = methods
      .filter(m => !m.isConstructor() && !Modifier.isStatic(m.getModifiers))
      .groupBy(_.getName.getIdentifier).map {
        case (name, methods) => {
          fromMethodDeclarations(methods)
        }
      }

    val staticMethods = methods
      .filter(m => !m.isConstructor() && Modifier.isStatic(m.getModifiers))
      .groupBy(_.getName.getIdentifier).map {
        case (name, methods) => fromMethodDeclarations(methods)
      }

    val innerInterfaces = types.filter(x => x.isInterface()).map {
        x => new FunctionDeclaration(new Identifier(x.getName.getIdentifier), List(), new BlockStatement(List()))
      }.toList

    // TODO: Member inner classes should probably defined as getters
    //val memberInnerCasses = types.filter(x => !Modifier.isStatic(x.getModifiers)).map { fromClassOrInterfaceDeclarationMember(_) }
    val staticInnerClasses = types
      .filter(x => Modifier.isStatic(x.getModifiers) && !x.isInterface)
      .map { x => fromTypeDeclaration(x) }

    val interfaces = td.resolveBinding.getInterfaces.map { x => new Identifier(x.getTypeDeclaration.getName) } toList

    val superClass = if (hasSuperclass) {
      val superClassType = td.getSuperclassType.asInstanceOf[dom.SimpleType]
      val superClassName = superClassType.getName.getFullyQualifiedName
      val superClassNames = superClassName.split('.')
      if (superClassNames.length > 1) {
        new MemberExpression(new Identifier(superClassNames(0)), new Identifier(superClassNames(1)), false)
      } else new Identifier(superClassName)
    } else null

    createClassDefinition(constructor, superClass, interfaces, memberMethods, staticMethods, innerInterfaces, staticInnerClasses, staticConstructor +: staticFields)
  }

  def createClassDefinition(
    constructor: FunctionExpression,
    superClass: Expression = null,
    interfaces: List[Identifier] = List(),
    methods: Iterable[FunctionDeclaration] = List(),
    staticMethods: Iterable[FunctionDeclaration] = List(),
    innerInterfaces: List[FunctionDeclaration] = List(),
    staticInnerClasses: Array[List[Statement]] = Array(),
    staticFields: Array[AssignmentExpression] = Array())(implicit td: dom.TypeDeclaration): List[Statement] = {

    val name = new Identifier(td.getName.getIdentifier)

    // TODO: does not have to be a function?
    val returnClassName = new ReturnStatement(name)
    val getClassFunc = new FunctionExpression(List(), new BlockStatement(List(returnClassName)), false)
    val getClassProperty = new MethodDefinition(new Identifier("getClass"), getClassFunc, "method", false, false)

    val interfacesProperty = createInterfacesProperty(interfaces)

    val classMembers = methods.map { x => new MethodDefinition(x.id, 
        new FunctionExpression(x.params, x.body), "method", false, false) }

    val classMembersStatic = staticMethods.map { x => new MethodDefinition(x.id, 
        new FunctionExpression(x.params, x.body), "method", false, true) }
    
    val apply = new MemberExpression("constructor_", "apply")
    var apply2 = new MemberExpression(name, apply)
    val call = new ExpressionStatement(new CallExpression(apply2, List(new ThisExpression, new Identifier("arguments"))))
    var superCallArguments = null // List(new SpreadElement(new Identifier("arguments")))
    var superCall = new ExpressionStatement(new CallExpression(new Super, superCallArguments))
    var statements = if (superClass == null) List(call) else List(superCall, call)
    val constructorExpression = new FunctionExpression(null, new BlockStatement(statements), false)
    
    val constructorMethod = new MethodDefinition(new Identifier("constructor"), constructorExpression, "constructor", false, false)
    val classBody = new ClassBody(List(constructorMethod) ++ classMembersStatic ++ classMembers :+ getClassProperty :+ interfacesProperty)
    val classDefinition = new ClassDeclaration(name, classBody, superClass)
    
    val innerInterfacesClassAssignments = innerInterfaces.map { x =>
      val memberName = new MemberExpression(name, x.id)
      val assignmentExpression = new AssignmentExpression("=", memberName, x.id)
      new ExpressionStatement(assignmentExpression)
    }

    val staticInnerClassAssignments = staticInnerClasses.map { x =>
      val id = x.head.asInstanceOf[ClassDeclaration].id
      val memberName = new MemberExpression(name, id)
      val assignmentExpression = new AssignmentExpression("=", memberName, id)
      new ExpressionStatement(assignmentExpression)
    }
    
    val staticFieldStatements = staticFields.map(new ExpressionStatement(_))

    List(classDefinition) ++
    innerInterfaces ++
    innerInterfacesClassAssignments ++ staticInnerClasses.flatten.toList ++ staticInnerClassAssignments ++ staticFieldStatements
  }

}