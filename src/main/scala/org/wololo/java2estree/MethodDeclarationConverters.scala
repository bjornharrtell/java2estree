package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import org.eclipse.jdt.core.dom
import Converters._
import ExpressionConversions._
import StatementConverters._

object MethodDefinitionConverters {
  def toArrowFunctionExpression(x: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) = 
    new ArrowFunctionExpression(
      toIdentifiers(x.parameters),
      toBlockStatement(x.getBody),
      false
    )
  
  def toFunctionExpression(x: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) =
    new FunctionExpression(
      toIdentifiers(x.parameters),
      toBlockStatement(x.getBody)
    )
  
  def varToBinaryExpression(x: dom.SingleVariableDeclaration) = {
    val identifier = resolveSimpleName(x.getName)
    val typeName = x.getType.resolveBinding().getName
    if (typeName == "int") {
      new MemberExpression(new Identifier("Number"), new CallExpression(new Identifier("isInteger"), List(identifier)), false)      
    } else if (typeName == "double") {
      new UnaryExpression("!", true, new MemberExpression(new Identifier("Number"), new CallExpression(new Identifier("isInteger"), List(identifier)), false))
    } else
      toInstanceOf(identifier, typeName)
  }
  
  def parseSameArgLength(declarations: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration) = {
    def convertTypeOverloads(mds: Iterable[dom.MethodDeclaration]) : Statement = {
      if (mds.size > 0) {
        val es = mds.head.parameters collect { case x: dom.SingleVariableDeclaration => varToBinaryExpression(x) }
        val test = if (es.size == 3) 
          new LogicalExpression("&&", es(2), new LogicalExpression("&&", es(0), es(1)))
        else if (es.size == 2) 
          new LogicalExpression("&&", es(0), es(1))
        else 
          es(0)
        val consequent = toBlockStatement(mds.head.getBody)
        new IfStatement(test, consequent, convertTypeOverloads(mds.tail))
      } else null
    }
    
    val arrowFunction = if (declarations.size > 1) {
      // TODO: need to create alias for parameters if they differ in name.. or cheat and fix that in the Java code..
      
      val body = new BlockStatement(List(convertTypeOverloads(declarations)))  
      new ArrowFunctionExpression(toIdentifiers(declarations.head.parameters), body, false)
    } else 
      toArrowFunctionExpression(declarations.head)
    
    val args = List(new SpreadElement(new Identifier("args")))
    new ReturnStatement(new CallExpression(arrowFunction, args))
  }
  
  def parseAll(x: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration) = {
    val cases = x.groupBy { _.parameters.length }.collect {
      case (k, v) => 
        new SwitchCase(new Literal(k, k.toString), List(parseSameArgLength(v)))
    }
    var switch = new SwitchStatement(
      new MemberExpression(new Identifier("args"), new Identifier("length"), false),
      cases
    )
    new BlockStatement(List(switch))
  }
  
  def fromMethodDeclaration(x: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) =
    new MethodDefinition(
      new Identifier(x.getName.getIdentifier),
      new FunctionExpression(toIdentifiers(x.parameters),
          toBlockStatement(x.getBody)),
      "method",
      false,
      dom.Modifier.isStatic(x.getModifiers)
    )
  
  def fromMethodDeclarationOverloads(x: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration) = {
    new MethodDefinition(
      new Identifier(x.head.getName.getIdentifier),
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
    new FunctionExpression(toIdentifiers(x.parameters),
        new BlockStatement(fieldInits ++ toBlockStatement(x.getBody).body)),
    "method",
    false,
    dom.Modifier.isStatic(x.getModifiers)
  )
  
  def fromConstructorDeclarationOverloads(x: Iterable[dom.MethodDeclaration], fieldInits: Iterable[Statement])(implicit td: dom.TypeDeclaration) = {  
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