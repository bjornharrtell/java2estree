package org.wololo.java2estree

import scala.collection.JavaConversions._

import org.wololo.estree._
import Converters._
import MethodDefinitionConverters._
import OperatorConversions._
import ExpressionConversions._
import StatementConverters._

import com.github.javaparser.{ ast => jp }

object CollectionConverters {
  def parameters(l: java.util.List[jp.body.Parameter]): List[Identifier] =
    l.toList map { a => identifier(a) }
  
  def statements(l: java.util.List[jp.stmt.Statement]): List[Statement] =
    if (l == null) List() else l.toList map { a => statement(a) }
  
  def expressions(l: java.util.List[jp.expr.Expression]): List[Expression] =
    if (l == null) List() else l.toList map { a => expression(a) }
  
  def typeDeclarations(l: java.util.List[jp.body.TypeDeclaration]):
    List[ClassDeclaration] = l.toList map { a => classDeclaration(a) }
  
  def bodyDeclarations(l: java.util.List[jp.body.BodyDeclaration]):
    List[MethodDefinition] = l.toList.groupBy(_.getClass).map {
    
    // single constructor
    case (key, value) if key == classOf[jp.body.ConstructorDeclaration] && value.length == 1 => 
      value.map { x => fromConstructorDeclaration(x.asInstanceOf[jp.body.ConstructorDeclaration]) }
     
    // multiple (overloaded) constructors
    case (key, value) if key == classOf[jp.body.ConstructorDeclaration] && value.length > 2 => 
      fromConstructorDeclarationOverloads(value.map { _.asInstanceOf[jp.body.ConstructorDeclaration] })
    
    // field declarations
    case (key, value) if key == classOf[jp.body.FieldDeclaration] => 
      value.map { x => fromFieldDeclaration(x.asInstanceOf[jp.body.FieldDeclaration]) } flatten
    
    // method declarations
    case (key, value) if key == classOf[jp.body.MethodDeclaration] && value.length == 1 => 
      List(fromMethodDeclaration(value(0).asInstanceOf[jp.body.MethodDeclaration]))
    
    // TODO: wrong.. need to move into above and group by name
    case (key, value) if key == classOf[jp.body.MethodDeclaration] && value.length > 1 => 
      fromMethodDeclarationOverloads(value.map { _.asInstanceOf[jp.body.MethodDeclaration] })
    
  }.toList flatten
  
  
  def variableDeclarators(l: java.util.List[jp.body.VariableDeclarator]):
    List[VariableDeclarator] = l.toList map { a => variableDeclarator(a) }
}