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
    val classDeclaration = types.head.asInstanceOf[ClassDeclaration]
    
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
    
    val exportedTypes = new ExportDefaultDeclaration(types.head) +: types.tail
    new Program("module", imports ++ exportedTypes)
  }
    
  /*def classExpression(td : dom.TypeDeclaration) = {
    val (body, statics) = toClassBody(td)
    new ClassExpression(body)
  }*/
  
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
  
  def createConstructor(hasSuper: Boolean) = {
    val args = List(new SpreadElement(new Identifier("args")))
    val init = new ExpressionStatement(
      new CallExpression(
        new MemberExpression(new ThisExpression(), new Identifier("init_"), false),
        args
      )
    )
    val superCall = new ExpressionStatement(new CallExpression(new Super(), List()))
    val statements = if (hasSuper) List(superCall, init) else List(init) 
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
  
  def createInitMethod(constructors: Array[dom.MethodDeclaration], hasSuperclass: Boolean)(implicit td: dom.TypeDeclaration): MethodDefinition = {
    val memberFields = td.getFields map { fromFieldDeclarationMember(_) } flatten
    
    val (params, statements) = constructors.length match {
      case x if x == 1 => (fromParameters(constructors.head.parameters), fromBlock(constructors.head.getBody).body)
      case x if x > 1 => (List(new RestElement(new Identifier("args"))), fromOverloadedMethodDeclarations(constructors).body)
      case _ => (List(), List())
    }
    
    val superInit = if (hasSuperclass) new ExpressionStatement(new CallExpression(new MemberExpression(new Super, new Identifier("init_"), false), List())) else null
    val defaultStatements = if (hasSuperclass) superInit +: memberFields else memberFields
    
    new MethodDefinition(
      new Identifier("init_"),
      new FunctionExpression(params, new BlockStatement(defaultStatements ++ statements)),
      "method",
      false,
      false
    )
  }
  
  def fromTypeDeclaration(implicit td: dom.TypeDeclaration): Iterable[Node] = {
    val methods = td.getMethods filterNot { x => Modifier.isAbstract(x.getModifiers) }
    val types = td.getTypes

    val constructors = methods filter { _.isConstructor() }
    val staticFields = td.getFields map { fromFieldDeclarationStatic(_) } flatten
    val hasSuperclass = td.getSuperclassType != null
    val initMethod = createInitMethod(constructors, hasSuperclass)
    val constructor = createConstructor(hasSuperclass)
    
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
    val staticInnerClasses = types.filter(x => Modifier.isStatic(x.getModifiers)).map { x => 
      val statements = fromTypeDeclaration(x).toList
      //val c = statements.head.asInstanceOf[ClassDeclaration]
      val a = new AssignmentExpression("=", new MemberExpression(new Identifier(td.getName.getIdentifier), new Identifier(x.getName.getIdentifier), false), new Identifier(x.getName.getIdentifier))
      statements :+ new ExpressionStatement(a)
    } flatten
    
    val body = new ClassBody(List(constructor, initMethod) ++ memberMethods ++ staticMethods)
    
    val superClass = if (hasSuperclass) new Identifier(td.getSuperclassType.resolveBinding.getName) else null
    val declaration = new ClassDeclaration(new Identifier(td.getName.getIdentifier), body, superClass)
    
    List(declaration) ++ staticInnerClasses ++ staticFields 
  }
}