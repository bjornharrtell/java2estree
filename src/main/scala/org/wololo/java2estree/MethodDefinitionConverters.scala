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
  
  def fromFieldDeclaration(declaration: jp.body.FieldDeclaration) = declaration.getVariables.toList map { field =>
    // TODO: need setter and private/hidden member in class if non-final
    new MethodDefinition(
      new Identifier(field.getId.getName),
      new FunctionExpression(List(), new BlockStatement(
          if (field.getInit == null) List() else
            List(new ReturnStatement(field.getInit)))),
      "get",
      false,
      Modifier.isStatic(declaration.getModifiers)
    )
  }
  
  def fromConstructorDeclaration(x: jp.body.ConstructorDeclaration) = new MethodDefinition(
    new Identifier("constructor"),
    new FunctionExpression(x.getParameters map identifier,
        blockStatement(x.getBlock)),
    "constructor",
    false,
    Modifier.isStatic(x.getModifiers)
  )
  
  def fromConstructorDeclarationOverloads(x: Iterable[jp.body.ConstructorDeclaration]) = x map { x => new MethodDefinition(
    new Identifier("constructor"),
      new FunctionExpression(x.getParameters map identifier,
          blockStatement(x.getBlock)),
      "constructor",
      false,
      Modifier.isStatic(x.getModifiers)
    )
  }
  
  def fromClassOrInterfaceDeclaration(x: jp.body.ClassOrInterfaceDeclaration) = new MethodDefinition(
    new Identifier(x.getName),
    new FunctionExpression(List(),
        new BlockStatement(List(new ReturnStatement(classExpression(x))))),
        //blockStatement(classDeclaration(x))),
    "get",
    false,
    Modifier.isStatic(x.getModifiers)
  )
}