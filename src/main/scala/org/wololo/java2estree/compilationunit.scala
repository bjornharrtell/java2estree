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
import com.google.common.io.Files
import java.io.File
import org.eclipse.jdt.core.dom.SuperConstructorInvocation
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.module.scala.DefaultScalaModule

object compilationunit {
  def fromCompilationUnit(cu : dom.CompilationUnit, path: String, filename: String): Program = {
    val types = cu.types.toList collect { case x: dom.TypeDeclaration => fromTypeDeclaration(x) } flatten;
    val classDeclaration = types.head
    val staticClassDeclarations = types.tail
    
    val mapper = new ObjectMapper
    mapper.registerModule(DefaultScalaModule)
    val tree = mapper.valueToTree[JsonNode](types)
    
    def countIdentifier(name: String) = 
      tree.findParents("type").filter { x => x.get("type").asText() == "Identifier" && x.get("name").asText() == name }.size
        
    val packageImports = if (cu.getPackage != null)
      importsFromName(cu.getPackage.getName.getFullyQualifiedName, path, filename)
    else 
      Map[String, String]()
          
    val explicitImports = cu.imports.toList collect { case x: dom.ImportDeclaration => fromImportDeclaration(x, path) };
    val distinctImports = (builtinImports ++ packageImports ++ explicitImports) - classDeclaration.id.name
    val usedImports = distinctImports.filter { case (name, path) => countIdentifier(name) > 0 }
    
    val imports = usedImports.collect { case (name, path) => createImport(name, path) }
    
    val exportedTypes = new ExportDefaultDeclaration(classDeclaration) +: staticClassDeclarations
    new Program("module", imports ++ exportedTypes)
  }
  
  def fromSingleVariableDeclaration(x: dom.SingleVariableDeclaration): Identifier =
    new Identifier(x.getName.getIdentifier)
  
  def fromParameters(parameters: java.util.List[_]) = 
    parameters collect { case x: dom.SingleVariableDeclaration => fromSingleVariableDeclaration(x) }
  
  def fromBlock(bs: dom.Block)(implicit td: dom.TypeDeclaration): BlockStatement =
    if (bs == null)
      new BlockStatement(List())
    else
      new BlockStatement(
        bs.statements collect { case statement: dom.Statement => fromStatement(statement)})
  
  def createConstructor(constructors: Array[dom.MethodDeclaration], memberFields: Array[ExpressionStatement], hasSuper: Boolean)(implicit td: dom.TypeDeclaration) = {
    val args = List(new SpreadElement(new Identifier("args")))
    val statements = createConstructorBody(constructors, memberFields, hasSuper)
    new MethodDefinition(
      new Identifier("constructor"),
      new FunctionExpression(
        List(new RestElement(new Identifier("args"))), new BlockStatement(statements)
      ),
      "constructor",
      false,
      false
    )
  }
  
  def createConstructorBody(constructors: Array[dom.MethodDeclaration], memberFields: Array[ExpressionStatement], hasSuper: Boolean)(implicit td: dom.TypeDeclaration): Iterable[Statement] = {
    val memberFields = td.getFields map { fromFieldDeclarationMember(_) } flatten
    val memberInitArrow = new ArrowFunctionExpression(
      List(),
      new BlockStatement(memberFields),
      false
    )
    val memberInitCall = new ExpressionStatement(new CallExpression(memberInitArrow, List()))

    val params = List(new RestElement(new Identifier("args")))
    val statements = fromOverloadedMethodDeclarations(constructors)
    
    val superCall = if (hasSuper) new ExpressionStatement(new CallExpression(new Super, List())) else null
    val defaultStatements = if (hasSuper) List(superCall, memberInitCall) else List(memberInitCall)
    
    defaultStatements ++ statements
  }
  
  def fromTypeDeclaration(implicit td: dom.TypeDeclaration): Array[ClassDeclaration] = {
    val methods = td.getMethods filterNot { x => Modifier.isAbstract(x.getModifiers) }
    val types = td.getTypes

    val constructors = methods filter { _.isConstructor() }
    val memberFields = td.getFields map { fromFieldDeclarationMember(_) } flatten
    val staticFields = td.getFields map { fromFieldDeclarationStatic(_) } flatten
    val hasSuperclass = td.getSuperclassType != null
    
    val constructor = if (constructors.size != 0 || memberFields.size != 0)
      createConstructor(constructors, memberFields, hasSuperclass)
    else
      null
    
    val memberMethods = methods.filter(m => !m.isConstructor() && !Modifier.isStatic(m.getModifiers)).groupBy(_.getName.getIdentifier).map {
      case (name, methods) if methods.length == 1 =>
        List(fromMethodDeclaration(methods.head))
      case (name, methods) if methods.length > 1 =>
        List(fromMethodDeclarationOverloads(methods))
    } flatten
    
    val staticMethods = methods.filter(m => !m.isConstructor() && Modifier.isStatic(m.getModifiers)).groupBy(_.getName.getIdentifier).map {
      case (name, methods) if methods.length == 1 =>
        List(fromMethodDeclaration(methods.head))
      case (name, methods) if methods.length > 1 =>
        List(fromMethodDeclarationOverloads(methods))
    } flatten
    
    // TODO: Member inner classes should probably defined as getters
    //val memberInnerCasses = types.filter(x => !Modifier.isStatic(x.getModifiers)).map { fromClassOrInterfaceDeclarationMember(_) }
    val staticInnerClasses = types.filter(x => Modifier.isStatic(x.getModifiers)).map { x => fromTypeDeclaration(x) } flatten
    val staticInnerClassProperties = types.filter(x => Modifier.isStatic(x.getModifiers)).map { x => 
      new MethodDefinition(
        new Identifier(x.getName.getIdentifier),
        new FunctionExpression(
            List(),
            new BlockStatement(List(new ReturnStatement(new Identifier(x.getName.getIdentifier))))
        ),
        "get",
        false,
        true
      )
    }
    
    val interfaces = td.resolveBinding.getInterfaces.map { x => new Identifier(x.getName) }
    val returnInterfaces = new ReturnStatement(new ArrayExpression(interfaces))
    val interfacesProperty = new MethodDefinition(
      new Identifier("interfaces_"),
      new FunctionExpression(List(), new BlockStatement(List(returnInterfaces))),
      "get",
      false,
      false
    )
    
    val init = if (constructor == null) List(interfacesProperty) else List(constructor, interfacesProperty)
    val body = new ClassBody(init ++ staticFields ++ staticInnerClassProperties ++ staticMethods ++ memberMethods)
    
    val superClass = if (hasSuperclass) new Identifier(td.getSuperclassType.resolveBinding.getName) else null
    new ClassDeclaration(new Identifier(td.getName.getIdentifier), body, superClass) +: staticInnerClasses
  }
}