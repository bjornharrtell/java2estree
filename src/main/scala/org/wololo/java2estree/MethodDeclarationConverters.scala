package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import org.eclipse.jdt.core.{ dom => jp }
import Converters._
import ExpressionConversions._
import StatementConverters._
import org.eclipse.jdt.core.dom.Modifier

object MethodDefinitionConverters {
  def toFunctionExpression(x: jp.MethodDeclaration)(implicit td: jp.TypeDeclaration) = new FunctionExpression(
    x.parameters map { p => identifier(p.asInstanceOf[jp.SingleVariableDeclaration])} ,
    blockStatement(x.getBody)
  )
  
  def fromMethodDeclaration(x: jp.MethodDeclaration)(implicit td: jp.TypeDeclaration) = new MethodDefinition(
    identifier(x.getName),
    new FunctionExpression(x.parameters map { p => identifier(p.asInstanceOf[jp.SingleVariableDeclaration])},
        blockStatement(x.getBody)),
    "method",
    false,
    Modifier.isStatic(x.getModifiers)
  )
 
  def fromMethodDeclarationOverloads(x: Iterable[jp.MethodDeclaration])(implicit td: jp.TypeDeclaration) = {
    
    def parseSameArgLength(declarations: Iterable[jp.MethodDeclaration])(implicit td: jp.TypeDeclaration) = {
      List(
        // TODO: consider multiple declarations, switch them on parameter type
        new ReturnStatement(new CallExpression(
          toFunctionExpression(declarations.head),
          List(new SpreadElement(new Identifier("args")))
        )),
        new ExpressionStatement(new Identifier("break"))
      )
    }
    
    def parseAll(x: Iterable[jp.MethodDeclaration]) : BlockStatement = {
      val cases = x.groupBy { _.parameters.length }.collect {
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
      identifier(x.head.getName),
      new FunctionExpression(
          List(new RestElement(new Identifier("args"))),
          parseAll(x)),
      "method",
      false,
      Modifier.isStatic(x.head.getModifiers)
    )
  }
  
  def fromFieldDeclarationMember(declaration: jp.FieldDeclaration)(implicit td: jp.TypeDeclaration) = 
    declaration.fragments collect {
      case field: jp.VariableDeclarationFragment if !Modifier.isStatic(declaration.getModifiers) =>
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
        new ThisExpression(), new Identifier(field.getName.getIdentifier), false),
        toExpression(field.getInitializer)))
  }
  
  def fromFieldDeclarationStatic(declaration: jp.FieldDeclaration)(implicit td: jp.TypeDeclaration) = 
    declaration.fragments collect { 
      case field: jp.VariableDeclarationFragment if Modifier.isStatic(declaration.getModifiers) =>
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
        new Identifier(field.resolveBinding().getDeclaringClass.getName),
        new Identifier(field.getName.getIdentifier), false),
        toExpression(field.getInitializer)))
  }
  
  def fromConstructorDeclaration(x: jp.MethodDeclaration, fieldInits: Iterable[Statement])(implicit td: jp.TypeDeclaration) = new MethodDefinition(
    new Identifier("init_"),
    new FunctionExpression(x.parameters map { p => identifier(p.asInstanceOf[jp.SingleVariableDeclaration])  },
        new BlockStatement(fieldInits ++ blockStatement(x.getBody).body)),
    "method",
    false,
    Modifier.isStatic(x.getModifiers)
  )
  
  def fromConstructorDeclarationOverloads(x: Iterable[jp.MethodDeclaration], fieldInits: Iterable[Statement])(implicit td: jp.TypeDeclaration) = {
    
    def parseSameArgLength(declarations: Iterable[jp.MethodDeclaration])(implicit td: jp.TypeDeclaration) = {
      List(
        // TODO: consider multiple declarations, switch them on parameter type
        new ReturnStatement(new CallExpression(
          toFunctionExpression(declarations.head),
          List(new SpreadElement(new Identifier("args")))
        )),
        new ExpressionStatement(new Identifier("break"))
      )
    }
    
    def parseAll(x: Iterable[jp.MethodDeclaration])(implicit td: jp.TypeDeclaration) : BlockStatement = {
      val cases = x.groupBy { _.parameters.length }.collect {
        case (k, v) => 
          new SwitchCase(new Literal(k, k.toString), parseSameArgLength(v))
      }
      var switch = new SwitchStatement(
        new MemberExpression(new Identifier("args"), new Identifier("length"), false),
        cases
      )
      new BlockStatement(List(switch))
    } 
    
    new MethodDefinition(new Identifier("init_"),
      new FunctionExpression(
          List(new RestElement(new Identifier("args"))),
          new BlockStatement(fieldInits ++ parseAll(x).body)
          ),
      "method",
      false,
      Modifier.isStatic(x.head.getModifiers)
    )
  }
  
  /*def fromClassOrInterfaceDeclarationMember(x: jp.body.ClassOrInterfaceDeclaration) : ExpressionStatement = 
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
    } else null*/
}