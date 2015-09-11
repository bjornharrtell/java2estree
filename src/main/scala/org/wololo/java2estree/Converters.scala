package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import org.eclipse.jdt.core.dom
import ExpressionConversions._
import StatementConverters._
import MethodDefinitionConverters._
import com.typesafe.scalalogging.LazyLogging
import org.eclipse.jdt.core.dom.Modifier

object Converters extends LazyLogging {
  def toProgram(cu : dom.CompilationUnit) : Program = {
    val imports = cu.imports.toList collect { case x: dom.ImportDeclaration => toModuleDeclarations(x) };
    val types = cu.types.toList collect { case x: dom.TypeDeclaration => toStatements(x) } flatten;
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
  
  //def toIdentifier(p: dom.SimpleName): Identifier = new Identifier(p.getIdentifier)
  
  def toVariableDeclarator(vd: dom.VariableDeclarationFragment)(implicit td: dom.TypeDeclaration) =
    new VariableDeclarator(new Identifier(vd.getName.getIdentifier), toExpression(vd.getInitializer))
  
  def toBlockStatement(bs: dom.Block)(implicit td: dom.TypeDeclaration): BlockStatement =
    if (bs == null)
      new BlockStatement(List())
    else
      new BlockStatement(
        bs.statements collect { case statement: dom.Statement => toStatement(statement)})
  
  def createConstructor = {
    new MethodDefinition(
      new Identifier("constructor"),
      new FunctionExpression(
        List(new RestElement(new Identifier("args"))),
        new BlockStatement(List(new ExpressionStatement(
            new CallExpression(
              new MemberExpression(new ThisExpression(), new Identifier("init_"), false),
              List(new SpreadElement(new Identifier("args"))
              ))))
        )),
      "constructor",
      false,
      false
    )
  }
  
  def toModuleDeclarations(implicit id: dom.ImportDeclaration): Node = {
    val orgname = id.getName.getFullyQualifiedName
    val path = orgname.replace('.', '/')
    val name = orgname.split('.').last
    val s = List(new ImportDefaultSpecifier(new Identifier(name)))
    new ImportDeclaration(s, new Literal(s"'${path}'", s"'${path}'"))
  }
  
  def toStatements(implicit td: dom.TypeDeclaration): Iterable[Node] = {
    
    val fields = td.getFields
    val methods = td.getMethods filterNot { x => Modifier.isAbstract(x.getModifiers) }
    val types = td.getTypes

    val memberFields = fields map { fromFieldDeclarationMember(_) } flatten
    val staticFields = fields map { fromFieldDeclarationStatic(_) } flatten
    
    // TODO: make simple constructor if there are no overloads
    
    val constructors = methods filter { _.isConstructor() }
    val initMethod = if (constructors.length == 1)
      fromConstructorDeclaration(constructors.head, memberFields)
      else if (constructors.length > 1) fromConstructorDeclarationOverloads(constructors, memberFields)
      else new MethodDefinition(
          new Identifier("init_"),
          new FunctionExpression(List(), new BlockStatement(List())),
          "method",
          false,
          false
      )
      
    val constructor = createConstructor
    
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
      val statements = toStatements(x).toList
      //val c = statements.head.asInstanceOf[ClassDeclaration]
      val a = new AssignmentExpression("=", new MemberExpression(new Identifier(td.getName.getIdentifier), new Identifier(x.getName.getIdentifier), false), new Identifier(x.getName.getIdentifier))
      statements :+ new ExpressionStatement(a)
    } flatten
    
    val body = new ClassBody(List(constructor, initMethod) ++ memberMethods ++ staticMethods)
    
    val superClass = if (td.getSuperclass != null) new Identifier(td.getSuperclass.getFullyQualifiedName) else null
    val declaration = new ClassDeclaration(new Identifier(td.getName.getIdentifier), body, superClass)
    
    List(declaration) ++ staticFields ++ staticInnerClasses
  }
}