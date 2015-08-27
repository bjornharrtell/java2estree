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
  def toFunctionExpression(x: jp.body.MethodDeclaration) = new FunctionExpression(
    x.getParameters map identifier,
    blockStatement(x.getBody)
  )
  
  def fromMethodDeclaration(x: jp.body.MethodDeclaration) = new MethodDefinition(
    new Identifier(x.getName),
    new FunctionExpression(x.getParameters map identifier,
        blockStatement(x.getBody)),
    "method",
    false,
    Modifier.isStatic(x.getModifiers)
  )
 
  def fromMethodDeclarationOverloads(x: Iterable[jp.body.MethodDeclaration]) = {
    
    def parseSameArgLength(declarations: Iterable[jp.body.MethodDeclaration]) = {
      List(
        // TODO: consider multiple declarations, switch them on parameter type
        new ReturnStatement(new CallExpression(
          toFunctionExpression(declarations.head),
          List(new SpreadElement(new Identifier("args")))
        )),
        new ExpressionStatement(new Identifier("break"))
      )
    }
    
    def parseAll(x: Iterable[jp.body.MethodDeclaration]) : BlockStatement = {
      val cases = x.groupBy { _.getParameters.length }.collect {
        case (k, v) => 
          new SwitchCase(new Literal(k, k.toString), parseSameArgLength(v))
      }
      var switch = new SwitchStatement(
        new MemberExpression(new Identifier("args"), new Identifier("length"), false),
        cases
      )
      new BlockStatement(List(switch))
    }
        
    new MethodDefinition(
    new Identifier(x.head.getName),
      new FunctionExpression(
          List(new RestElement(new Identifier("args"))),
          parseAll(x)),
      "method",
      false,
      false
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