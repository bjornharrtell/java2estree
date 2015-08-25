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
  def program(cu : jp.CompilationUnit) : Program =
    new Program("module",
        if (cu.getTypes == null) List() else cu.getTypes map classDeclaration)
  
  def classDeclaration(td : jp.body.TypeDeclaration) =
    new ClassDeclaration(new Identifier(td.getName), classBody(td.getMembers))
  
  def classExpression(cd : jp.body.ClassOrInterfaceDeclaration) =
    new ClassExpression(classBody(cd.getMembers))
  
  def identifier(p : jp.body.Parameter) = new Identifier(p.getId.getName)
  
  def variableDeclarator(vd: jp.body.VariableDeclarator) =
    new VariableDeclarator(new Identifier(vd.getId.getName), vd.getInit)
  
  def blockStatement(bs: jp.stmt.BlockStmt) =
    new BlockStatement(
        if (bs.getStmts == null) List() else bs.getStmts map statement)  
  
  def classBody(l: java.util.List[jp.body.BodyDeclaration]):
    ClassBody = new ClassBody(l.groupBy(_.getClass).map {
    
    // single constructor
    case (g, l) if g == classOf[jp.body.ConstructorDeclaration] && l.length == 1 => 
      l.map { x => fromConstructorDeclaration(x.asInstanceOf[jp.body.ConstructorDeclaration]) }
     
    // multiple (overloaded) constructors
    case (g, l) if g == classOf[jp.body.ConstructorDeclaration] && l.length > 1 => 
      fromConstructorDeclarationOverloads(l.map { _.asInstanceOf[jp.body.ConstructorDeclaration] })
    
    // field declarations
    case (g, l) if g == classOf[jp.body.FieldDeclaration] => 
      l.map { x => fromFieldDeclaration(x.asInstanceOf[jp.body.FieldDeclaration]) } flatten
    
    // method declarations
    case (g, l) if g == classOf[jp.body.MethodDeclaration] => 
      l.groupBy(x => x.asInstanceOf[jp.body.MethodDeclaration].getName).map {
      
      // single method declaration
      case (g, l) if l.length == 1 =>
        List(fromMethodDeclaration(l(0).asInstanceOf[jp.body.MethodDeclaration]))
      
      // overloaded method declarations
      case (g, l) if l.length > 1 =>
        fromMethodDeclarationOverloads(l.map { _.asInstanceOf[jp.body.MethodDeclaration] })
     
    } flatten
    
    case (g, l) if g == classOf[jp.body.ClassOrInterfaceDeclaration] =>
      l.map { x => fromClassOrInterfaceDeclaration(x.asInstanceOf[jp.body.ClassOrInterfaceDeclaration])}
      
  } flatten)
}