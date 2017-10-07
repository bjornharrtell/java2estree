package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
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

object compilationunit extends LazyLogging {

  def isDeprecated(javadoc: dom.Javadoc): Boolean = {
    if (javadoc != null && javadoc.tags != null)
      javadoc.tags.toList collect { case x: dom.TagElement => x } exists { x => x.getTagName == "@deprecated" }
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
    val types = cu.types.toList collect { case x: dom.TypeDeclaration if !isDeprecated(x.getJavadoc) => fromTypeDeclaration(x) } flatten

    if (types.length == 0) return null

    val functionDeclaration = types.head.asInstanceOf[FunctionDeclaration]
    val staticClassDeclarations = types.tail

    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    val tree = mapper.valueToTree[JsonNode](types)

    def countIdentifier(name: String) = {
      tree.findParents("type").filter {
        x => x.get("type").asText() == "Identifier" && x.get("name").asText() == name
      }.size
    }

    val packageImports = if (cu.getPackage != null)
      importsFromName(cu.getPackage.getName.getFullyQualifiedName, root, name)
    else
      Map[String, String]()

    val explicitImports = cu.imports.toList collect { case x: dom.ImportDeclaration => fromImportDeclaration(x, root, file) }
    val distinctImports = (builtinImports(root, file) ++ packageImports ++ explicitImports) - functionDeclaration.id.name
    //val distinctImports = (packageImports ++ explicitImports) - classDeclaration.id.name
    val usedImports = distinctImports.filter { case (name, path) => countIdentifier(name) > 0 }

    val imports = usedImports.collect { case (name, path) => createImport(name, path) }

    val exportedTypes = new ExportDefaultDeclaration(functionDeclaration) +: staticClassDeclarations
    new Program("module", imports ++ exportedTypes)
  }

  def fromSingleVariableDeclaration(x: dom.SingleVariableDeclaration): Identifier =
    new Identifier(x.getName.getIdentifier)

  def fromParameters(parameters: java.util.List[_]) =
    parameters collect { case x: dom.SingleVariableDeclaration => fromSingleVariableDeclaration(x) }

  def fromBlock2(bs: dom.Block)(implicit td: dom.TypeDeclaration): Iterable[Statement] =
    bs.statements collect { case statement: dom.Statement => fromStatement(statement) }

  def fromBlock(bs: dom.Block)(implicit td: dom.TypeDeclaration): BlockStatement =
    if (bs == null)
      new BlockStatement(List())
    else
      new BlockStatement(
        bs.statements collect { case statement: dom.Statement => fromStatement(statement) })

  def createConstructor(constructors: Array[dom.MethodDeclaration], memberFields: Array[ExpressionStatement], hasSuper: Boolean)(implicit td: dom.TypeDeclaration) = {
    val statements = createConstructorBody(constructors, memberFields, hasSuper)
    val params = List()
    new FunctionDeclaration(
      new Identifier(td.getName.getIdentifier),
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
      val superClass = td.getSuperclassType.asInstanceOf[dom.SimpleType].getName.getFullyQualifiedName
      val apply = new MemberExpression(superClass, "apply")
      val call = new CallExpression(apply, List(new ThisExpression))
      new ExpressionStatement(call) +: memberFields
    } else {
      memberFields
    }

    defaultStatements ++ statements
  }

  def createInterfacesProperty(interfaces: List[Identifier]): Property = {
    val returnInterfaces = new ReturnStatement(new ArrayExpression(interfaces))
    new Property(new Identifier("interfaces_"), new FunctionExpression(List(), new BlockStatement(List(returnInterfaces))))
  }

  def fromTypeDeclaration(implicit td: dom.TypeDeclaration): List[Statement] = {
    val methods = td.getMethods filterNot { x => Modifier.isAbstract(x.getModifiers) || isDeprecated(x.getJavadoc) }
    val types = td.getTypes

    val constructors = methods filter { m => m.isConstructor() }
    val memberFields = td.getFields map { fromFieldDeclarationMember(_) } flatten
    val staticFields = td.getFields map { fromFieldDeclarationStatic(_) } flatten

    val hasSuperclass = td.getSuperclassType != null

    val constructor = createConstructor(constructors, memberFields, hasSuperclass)

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

    val innerInterfaces = types.filter(x => x.isInterface()).map { x => new FunctionDeclaration(new Identifier(x.getName.getIdentifier), List(), new BlockStatement(List())) }.toList

    // TODO: Member inner classes should probably defined as getters
    //val memberInnerCasses = types.filter(x => !Modifier.isStatic(x.getModifiers)).map { fromClassOrInterfaceDeclarationMember(_) }
    val staticInnerClasses = types
      .filter(x => Modifier.isStatic(x.getModifiers) && !x.isInterface)
      .map { x => fromTypeDeclaration(x) }

    val interfaces = td.resolveBinding.getInterfaces.map { x => new Identifier(x.getName) } toList

    val superClass = if (hasSuperclass) {
      val superClassType = td.getSuperclassType.asInstanceOf[dom.SimpleType]
      val superClassName = superClassType.getName.getFullyQualifiedName
      val superClassNames = superClassName.split('.')
      if (superClassNames.length > 1) {
        new MemberExpression(new Identifier(superClassNames(0)), new Identifier(superClassNames(1)), false)
      } else new Identifier(superClassName)
    } else null

    createClassDefinition(constructor, superClass, interfaces, memberMethods, staticMethods, innerInterfaces, staticInnerClasses, staticFields)
  }

  def createClassDefinition(
    constructor: FunctionDeclaration,
    superClass: Expression = null,
    interfaces: List[Identifier] = List(),
    methods: Iterable[FunctionDeclaration] = List(),
    staticMethods: Iterable[FunctionDeclaration] = List(),
    innerInterfaces: List[FunctionDeclaration] = List(),
    staticInnerClasses: Array[List[Statement]] = Array(),
    staticFields: Array[AssignmentExpression] = Array()): List[Statement] = {

    val name = constructor.id

    // TODO: does not have to be a function?
    val returnClassName = new ReturnStatement(name)
    val getClassFunc = new FunctionExpression(List(), new BlockStatement(List(returnClassName)), false)
    val getClassProperty = new Property("getClass", getClassFunc)

    val interfacesProperty = createInterfacesProperty(interfaces)

    val properties = methods.map { x => new Property(x.id, new FunctionExpression(x.params, x.body)) }.toList ++ List(interfacesProperty, getClassProperty)
    val membersObject = new ObjectExpression(properties)
    val prototype = new MemberExpression(name, "prototype")
    val extend = new ExpressionStatement(new CallExpression(new Identifier("extend"), List(prototype, membersObject)))

    val classDefinition = if (superClass != null) {
      val inherits = new ExpressionStatement(new CallExpression(new Identifier("inherits"), List(name, superClass)))
      List(constructor, inherits, extend)
    } else List(constructor, extend)

    val innerInterfacesClassAssignments = innerInterfaces.map { x =>
      val memberName = new MemberExpression(name, x.id)
      val assignmentExpression = new AssignmentExpression("=", memberName, x.id)
      new ExpressionStatement(assignmentExpression)
    }

    val staticInnerClassAssignments = staticInnerClasses.map { x =>
      val id = x.head.asInstanceOf[FunctionDeclaration].id
      val memberName = new MemberExpression(name, id)
      val assignmentExpression = new AssignmentExpression("=", memberName, id)
      new ExpressionStatement(assignmentExpression)
    }

    val staticMethodAssignments = staticMethods.map { x =>
      val memberName = new MemberExpression(name, x.id)
      val functionExpression = new FunctionExpression(x.params, x.body)
      val assignmentExpression = new AssignmentExpression("=", memberName, functionExpression)
      new ExpressionStatement(assignmentExpression)
    }

    val staticFieldStatements = staticFields.map(new ExpressionStatement(_))

    classDefinition ++ 
    staticMethodAssignments ++ 
    innerInterfaces ++
    innerInterfacesClassAssignments ++ staticInnerClasses.flatten.toList ++ staticInnerClassAssignments ++ staticFieldStatements
  }

}