package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import org.eclipse.jdt.core.dom
import ExpressionConversions._
import StatementConverters._
import MethodDefinitionConverters._
import com.typesafe.scalalogging.LazyLogging
import org.eclipse.jdt.core.dom.Modifier
import com.google.common.io.Files
import java.io.File
import org.eclipse.jdt.core.dom.SuperConstructorInvocation
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.module.scala.DefaultScalaModule

object Converters extends LazyLogging {
  def toProgram(cu : dom.CompilationUnit, path: String, filename: String) : Program = {
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
    
    val imports = usedImports.collect { case (name, path) => defImport(name, path) }
    
    val exportedTypes = new ExportDefaultDeclaration(types.head) +: types.tail
    new Program("module", imports ++ exportedTypes)
  }
    
  /*def classExpression(td : dom.TypeDeclaration) = {
    val (body, statics) = toClassBody(td)
    new ClassExpression(body)
  }*/
  
  def toIdentifier(x: dom.SingleVariableDeclaration): Identifier =
    new Identifier(x.getName.getIdentifier)
  
  def toIdentifiers(parameters: java.util.List[_]) =
    parameters collect { case x: dom.SingleVariableDeclaration => toIdentifier(x) }
  
  def toVariableDeclarator(vd: dom.VariableDeclarationFragment)(implicit td: dom.TypeDeclaration) =
    new VariableDeclarator(new Identifier(vd.getName.getIdentifier), toExpression(vd.getInitializer))
  
  def toBlockStatement(bs: dom.Block)(implicit td: dom.TypeDeclaration): BlockStatement =
    if (bs == null)
      new BlockStatement(List())
    else
      new BlockStatement(
        bs.statements collect { case statement: dom.Statement => toStatement(statement)})
  
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
  
  def defImport(name: String, path: String) = {
    new ImportDeclaration(
        List(new ImportDefaultSpecifier(new Identifier(name))),
          new Literal(s"'${path}'", s"'${path}'"))
  }
  
  def builtinImports() : Map[String, String] = {
    Map(
      ("Comparable" -> "java/lang/Comparable"),
      ("Cloneable" -> "java/lang/Cloneable"),
      ("Character" -> "java/lang/Character"),
      ("Double" -> "java/lang/Double"),
      ("Exception" -> "java/lang/Exception"),
      ("RuntimeException" -> "java/lang/RuntimeException")
    )
  }
  
  def importsFromName(name: String, path: String, ignore: String = null) : Map[String, String] = {
    val subpath = name.replace('.', '/')
    val subname = name.split('.').last
 
    def isJava(file: File): Boolean = {
      val split = file.getName.split('.')
      if (split.length != 2) return false
      if (split(1) == "java") return true
      return false
    }
    
    val file = new File(path + '/' + subpath)
    val files = file.listFiles
    if (files == null) return Map()
    
    val pairs = files.filter({ x => x.getName.split('.')(0) != ignore }).collect { case x if isJava(x) => {
      val name = x.getName.split('.')(0)
      val path = subpath + '/' + name
      (name -> path)
    } }
    
    Map(pairs: _*)
  }
  
  def fromImportDeclaration(id: dom.ImportDeclaration, path: String): (String, String) = {
    val orgname = id.getName.getFullyQualifiedName
    val path = orgname.replace('.', '/')
    val name = orgname.split('.').last
    (name -> path)
  }
  
  def createInitMethod(constructors: Array[dom.MethodDeclaration], hasSuperclass: Boolean)(implicit td: dom.TypeDeclaration): MethodDefinition = {
    val memberFields = td.getFields map { fromFieldDeclarationMember(_) } flatten
    
    val (params, statements) = constructors.length match {
      case x if x == 1 => (toIdentifiers(constructors.head.parameters), toBlockStatement(constructors.head.getBody).body)
      case x if x > 1 => (List(new RestElement(new Identifier("args"))), parseAll(constructors).body)
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