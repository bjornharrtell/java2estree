package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import org.eclipse.jdt.core.{ dom => jp }
import ExpressionConversions._
import StatementConverters._
import MethodDefinitionConverters._
import com.typesafe.scalalogging.LazyLogging
import org.eclipse.jdt.core.dom.Modifier

object Converters extends LazyLogging {
  def program(cu : jp.CompilationUnit) : Program = {
    val sss = if (cu.types == null) List() else toStatements(cu.types.get(0).asInstanceOf[jp.TypeDeclaration])
    new Program("module", sss)
  }
    
  /*def classExpression(td : jp.TypeDeclaration) = {
    val (body, statics) = toClassBody(td)
    new ClassExpression(body)
  }*/
  
  def identifier(p: jp.SingleVariableDeclaration): Identifier = identifier(p.getName)
  
  def identifier(p: jp.SimpleName): Identifier = new Identifier(p.getIdentifier)
  
  def variableDeclarator(vd: jp.VariableDeclarationFragment)(implicit td: jp.TypeDeclaration) =
    new VariableDeclarator(identifier(vd.getName), toExpression(vd.getInitializer))
  
  def blockStatement(bs: jp.Block)(implicit td: jp.TypeDeclaration) =
    if (bs == null)
      new BlockStatement(List())
    else
      new BlockStatement(
        bs.statements collect { case statement: jp.Statement => toStatement(statement)})
  
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
  
  /**
   * @return A tuple with a ClassBody and any static statements that found in the BodyDeclaration
   */
  def toStatements(implicit td: jp.TypeDeclaration): Iterable[Statement] = {
    
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
      val c = statements.head.asInstanceOf[ClassDeclaration]
      val a = new AssignmentExpression("=", new MemberExpression(new Identifier(td.getName.getIdentifier), new Identifier(x.getName.getIdentifier), false), new Identifier(x.getName.getIdentifier))
      statements :+ new ExpressionStatement(a)
    } flatten
    
    val body = new ClassBody(List(constructor, initMethod) ++ memberMethods ++ staticMethods)
    val declaration = new ClassDeclaration(new Identifier(td.getName.getIdentifier), body)
    
    List(declaration) ++ staticFields ++ staticInnerClasses
  }
}