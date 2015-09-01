package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import org.eclipse.jdt.core.dom
import Converters._
import ExpressionConversions._
import StatementConverters._

object MethodDefinitionConverters {
  def toArrowFunctionExpression(x: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) = new ArrowFunctionExpression(
    x.parameters map { p => identifier(p.asInstanceOf[dom.SingleVariableDeclaration])} ,
    blockStatement(x.getBody),
    false
  )
  
  def toFunctionExpression(x: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) = new FunctionExpression(
    x.parameters map { p => identifier(p.asInstanceOf[dom.SingleVariableDeclaration])} ,
    blockStatement(x.getBody)
  )
  
  def fromMethodDeclaration(x: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) = new MethodDefinition(
    identifier(x.getName),
    new FunctionExpression(x.parameters map { p => identifier(p.asInstanceOf[dom.SingleVariableDeclaration])},
        blockStatement(x.getBody)),
    "method",
    false,
    dom.Modifier.isStatic(x.getModifiers)
  )
 
  def fromMethodDeclarationOverloads(x: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration) = {
    
    def parseSameArgLength(declarations: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration) = {
      List(
        // TODO: consider multiple declarations, switch them on parameter type
        new ReturnStatement(new CallExpression(
          toArrowFunctionExpression(declarations.head),
          List(new SpreadElement(new Identifier("args")))
        ))//,
        //new ExpressionStatement(new Identifier("break"))
      )
    }
    
    def parseAll(x: Iterable[dom.MethodDeclaration]) : BlockStatement = {
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
      dom.Modifier.isStatic(x.head.getModifiers)
    )
  }
  
  def fromFieldDeclarationMember(declaration: dom.FieldDeclaration)(implicit td: dom.TypeDeclaration) = 
    declaration.fragments collect {
      case field: dom.VariableDeclarationFragment if !dom.Modifier.isStatic(declaration.getModifiers) =>
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
        new ThisExpression(), new Identifier(field.getName.getIdentifier), false),
        toExpression(field.getInitializer)))
  }
  
  def fromFieldDeclarationStatic(declaration: dom.FieldDeclaration)(implicit td: dom.TypeDeclaration) = 
    declaration.fragments collect { 
      case field: dom.VariableDeclarationFragment if dom.Modifier.isStatic(declaration.getModifiers) =>
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
        new Identifier(field.resolveBinding().getDeclaringClass.getName),
        new Identifier(field.getName.getIdentifier), false),
        toExpression(field.getInitializer)))
  }
  
  def fromConstructorDeclaration(x: dom.MethodDeclaration, fieldInits: Iterable[Statement])(implicit td: dom.TypeDeclaration) = new MethodDefinition(
    new Identifier("init_"),
    new FunctionExpression(x.parameters map { p => identifier(p.asInstanceOf[dom.SingleVariableDeclaration])  },
        new BlockStatement(fieldInits ++ blockStatement(x.getBody).body)),
    "method",
    false,
    dom.Modifier.isStatic(x.getModifiers)
  )
  
  def fromConstructorDeclarationOverloads(x: Iterable[dom.MethodDeclaration], fieldInits: Iterable[Statement])(implicit td: dom.TypeDeclaration) = {
    
    def parseSameArgLength(declarations: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration) = {
      List(
        // TODO: consider multiple declarations, switch them on parameter type
        new ReturnStatement(new CallExpression(
          toArrowFunctionExpression(declarations.head),
          List(new SpreadElement(new Identifier("args")))
        ))//,
        //new ExpressionStatement(new Identifier("break"))
      )
    }
    
    def parseAll(x: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration) : BlockStatement = {
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
      dom.Modifier.isStatic(x.head.getModifiers)
    )
  }
  
  /*def fromClassOrInterfaceDeclarationMember(x: dom.TypeDeclaration)(implicit td: dom.TypeDeclaration) : ExpressionStatement = 
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
        new ThisExpression(),
        new Identifier(x.getName.getIdentifier), false),
        classExpression(x)))
    
  def fromClassOrInterfaceDeclarationStatic(x: dom.TypeDeclaration)(implicit td: dom.TypeDeclaration) : ExpressionStatement = 
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
        new Identifier(td.getName.getIdentifier),
        new Identifier(x.getName.getIdentifier), false),
        classExpression(x)))*/
}