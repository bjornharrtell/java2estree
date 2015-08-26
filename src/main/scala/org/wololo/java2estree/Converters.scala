package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import com.github.javaparser.{ ast => jp }
import java.lang.reflect.Modifier
import OperatorConversions._
import ExpressionConversions._
import StatementConverters._
import MethodDefinitionConverters._

import com.typesafe.scalalogging.LazyLogging

object Converters extends LazyLogging {
  def program(cu : jp.CompilationUnit) : Program = {
    val sss = if (cu.getTypes == null) List() else classDeclaration(cu.getTypes.get(0))
    new Program("module", sss)
  }
  
  def classDeclaration(td : jp.body.TypeDeclaration): Iterable[Statement] = {
    val (body, statics) = classBody(td.getMembers)
    val declaration = new ClassDeclaration(new Identifier(td.getName), body)
    List(declaration) ++ statics
  }
    
  def classExpression(cd : jp.body.ClassOrInterfaceDeclaration) = {
    val (body, statics) = classBody(cd.getMembers)
    new ClassExpression(body)
  }
  
  def identifier(p : jp.body.Parameter) = new Identifier(p.getId.getName)
  
  def variableDeclarator(vd: jp.body.VariableDeclarator) =
    new VariableDeclarator(new Identifier(vd.getId.getName), vd.getInit)
  
  def blockStatement(bs: jp.stmt.BlockStmt) =
    new BlockStatement(
        if (bs == null || bs.getStmts == null) List() else bs.getStmts map statement)  
  
  /**
   * @return A tuple with a ClassBody and any static statements that found in the BodyDeclaration
   */
  def classBody(l: java.util.List[jp.body.BodyDeclaration]): (ClassBody, Iterable[Statement]) = {
    val types = l.groupBy(_.getClass)
    
    val memberFields = types.collect {
      case (g, l) if g == classOf[jp.body.FieldDeclaration] => 
        l.map { x => fromFieldDeclarationMember(x.asInstanceOf[jp.body.FieldDeclaration]) } flatten
    } flatten
    
    val staticFields = types.collect {
      case (g, l) if g == classOf[jp.body.FieldDeclaration] => 
        l.map { x => fromFieldDeclarationStatic(x.asInstanceOf[jp.body.FieldDeclaration]) } flatten
    } flatten
    
    val constructor = types.collect {
      case (g, l) if g == classOf[jp.body.ConstructorDeclaration] && l.length == 1 => 
        List(fromConstructorDeclaration(l(0).asInstanceOf[jp.body.ConstructorDeclaration], memberFields))
      case (g, l) if g == classOf[jp.body.ConstructorDeclaration] && l.length > 1 => 
        fromConstructorDeclarationOverloads(l.map { _.asInstanceOf[jp.body.ConstructorDeclaration] }, memberFields)
    } flatten
    
    val methods = types.collect {
      case (g, l) if g == classOf[jp.body.MethodDeclaration] => 
        l.groupBy(x => x.asInstanceOf[jp.body.MethodDeclaration].getName).map {
        case (g, l) if l.length == 1 =>
          List(fromMethodDeclaration(l(0).asInstanceOf[jp.body.MethodDeclaration]))
        case (g, l) if l.length > 1 =>
          List(fromMethodDeclarationOverloads(l.map { _.asInstanceOf[jp.body.MethodDeclaration] }))
      } flatten
    } flatten
    
    val memberInnerCasses = types.collect {
      case (g, l) if g == classOf[jp.body.ClassOrInterfaceDeclaration] =>
        l.map { x => 
          fromClassOrInterfaceDeclarationMember(x.asInstanceOf[jp.body.ClassOrInterfaceDeclaration])
        }.filter(_ != null)
    } flatten
    
    val staticInnerClasses = types.collect {
      case (g, l) if g == classOf[jp.body.ClassOrInterfaceDeclaration] =>
        val ll = l.map { x => 
          fromClassOrInterfaceDeclarationStatic(x.asInstanceOf[jp.body.ClassOrInterfaceDeclaration])
        }.filter(_ != null)
        ll
    } flatten
    
    (
        new ClassBody(constructor ++ methods),// ++ classes),
        staticFields ++ staticInnerClasses
    )
  }
}