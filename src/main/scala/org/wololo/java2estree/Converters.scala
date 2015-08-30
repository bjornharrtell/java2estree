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
    val sss = if (cu.types == null) List() else classDeclaration(cu.types.get(0).asInstanceOf[jp.TypeDeclaration])
    new Program("module", sss)
  }
  
  def classDeclaration(td: jp.TypeDeclaration): Iterable[Statement] = {
    val (body, statics) = toClassBody(td)
    val declaration = new ClassDeclaration(new Identifier(td.getName.getIdentifier), body)
    List(declaration) ++ statics
  }
    
  def classExpression(td : jp.TypeDeclaration) = {
    val (body, statics) = toClassBody(td)
    new ClassExpression(body)
  }
  
  def identifier(p: jp.SingleVariableDeclaration): Identifier = identifier(p.getName)
  def identifier(p: jp.SimpleName): Identifier = new Identifier(p.getIdentifier)
  
  def variableDeclarator(vd: jp.VariableDeclarationFragment)(implicit td: jp.TypeDeclaration) =
    new VariableDeclarator(identifier(vd.getName), toExpression(vd.getInitializer))
  
  def blockStatement(bs: jp.Block)(implicit td: jp.TypeDeclaration) =
    new BlockStatement(
        bs.statements map { statement => toStatement(statement.asInstanceOf[jp.Statement])})
  
  def createConstructor() = {
    new MethodDefinition(
      new Identifier("constructor"),
      new FunctionExpression(
        List(new RestElement(new Identifier("args"))),
        new BlockStatement(List(new ExpressionStatement(
            new CallExpression(
                new Identifier("init_"),
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
  def toClassBody(implicit td: jp.TypeDeclaration): (ClassBody, Iterable[Statement]) = {
    
    val fields = td.getFields
    val methods = td.getMethods
    val types = td.getTypes

    val memberFields = fields map { fromFieldDeclarationMember(_) } flatten
    val staticFields = fields map { fromFieldDeclarationStatic(_) } flatten
    
    // TODO: make simple constructor if there is no overloads
    
    val constructor = createConstructor()
    
    val constructors = methods filter { _.isConstructor() }
    val initMethod = if (constructors.length == 1)
      List(fromConstructorDeclaration(constructors.head, memberFields))
      else if (constructors.length > 1) List(fromConstructorDeclarationOverloads(constructors, memberFields))
      else List()
    
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
   
    
    val memberInnerCasses = types.filter(x => Modifier.isStatic(x.getModifiers)).map { fromClassOrInterfaceDeclarationMember(_) }
    val staticInnerClasses = types.filter(x => Modifier.isStatic(x.getModifiers)).map { fromClassOrInterfaceDeclarationStatic(_) }
        
    (
        new ClassBody((constructor +: initMethod) ++ memberMethods ++ staticMethods),// ++ classes),
        staticFields ++ staticInnerClasses
    )
  }
}