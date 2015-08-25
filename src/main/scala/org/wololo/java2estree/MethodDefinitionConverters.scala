package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import com.github.javaparser.{ ast => jp }
import java.lang.reflect.Modifier
import Converters._
import OperatorConversions._
import ExpressionConversions._
import StatementConverters._

object MethodDefinitionConverters {
  def fromMethodDeclaration(x: jp.body.MethodDeclaration) = new MethodDefinition(
    new Identifier(x.getName),
    new FunctionExpression(x.getParameters map identifier,
        blockStatement(x.getBody)),
    "method",
    false,
    Modifier.isStatic(x.getModifiers)
  )
 
  def fromMethodDeclarationOverloads(x: Iterable[jp.body.MethodDeclaration]) = x map { x => new MethodDefinition(
    new Identifier(x.getName),
      new FunctionExpression(x.getParameters map identifier,
          blockStatement(x.getBody)),
      "method",
      false,
      Modifier.isStatic(x.getModifiers)
    )
  }
  
  def fromFieldDeclarationMember(declaration: jp.body.FieldDeclaration) = 
    declaration.getVariables.toList collect {
      case field if !Modifier.isStatic(declaration.getModifiers) =>
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
        new ThisExpression(), new Identifier(field.getId.getName), false),
        field.getInit))
  }
  
  def fromFieldDeclarationStatic(declaration: jp.body.FieldDeclaration) = 
    declaration.getVariables.toList collect { 
      case field if Modifier.isStatic(declaration.getModifiers) =>
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
        new Identifier(declaration.getParentNode.asInstanceOf[jp.body.ClassOrInterfaceDeclaration].getName),
        new Identifier(field.getId.getName), false),
        field.getInit))
  }
  
  def fromConstructorDeclaration(x: jp.body.ConstructorDeclaration, fieldInits: Iterable[Statement]) = new MethodDefinition(
    new Identifier("constructor"),
    new FunctionExpression(x.getParameters map identifier,
        new BlockStatement(fieldInits ++ blockStatement(x.getBlock).body)),
    "constructor",
    false,
    Modifier.isStatic(x.getModifiers)
  )
  
  def fromConstructorDeclarationOverloads(x: Iterable[jp.body.ConstructorDeclaration], fieldInits: Iterable[Statement]) = x map { x => new MethodDefinition(
    new Identifier("constructor"),
      new FunctionExpression(x.getParameters map identifier,
          new BlockStatement(fieldInits ++ blockStatement(x.getBlock).body)),
      "constructor",
      false,
      Modifier.isStatic(x.getModifiers)
    )
  }
  
  def fromClassOrInterfaceDeclarationMember(x: jp.body.ClassOrInterfaceDeclaration) : ExpressionStatement = 
    if (!Modifier.isStatic(x.getModifiers)) {
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
        new ThisExpression(),
        new Identifier(x.getName), false),
        classExpression(x)))
    } else null
    
  def fromClassOrInterfaceDeclarationStatic(x: jp.body.ClassOrInterfaceDeclaration) : ExpressionStatement = 
    if (Modifier.isStatic(x.getModifiers)) {
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
        new Identifier(x.getParentNode.asInstanceOf[jp.body.ClassOrInterfaceDeclaration].getName),
        new Identifier(x.getName), false),
        classExpression(x)))
    } else null
}