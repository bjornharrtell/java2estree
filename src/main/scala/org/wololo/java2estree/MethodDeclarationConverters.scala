package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import org.eclipse.jdt.core.dom
import Converters._
import ExpressionConversions._
import StatementConverters._

object MethodDefinitionConverters {
  def toFunctionExpression(x: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) =
    new FunctionExpression(
      toIdentifiers(x.parameters),
      toBlockStatement(x.getBody)
    )
  
  def varToBinaryExpression(x: dom.SingleVariableDeclaration, i: Int) = {
    val identifier = new MemberExpression(new Identifier("args"), new Literal(i, i.toString), true)
    val typeName = x.getType.resolveBinding().getName
    if (x.getType.isArrayType())
      toInstanceOf(identifier, "Array")
    else if (typeName == "int")
      new MemberExpression(new Identifier("Number"), new CallExpression(new Identifier("isInteger"), List(identifier)), false)      
    else if (typeName == "double")
      new UnaryExpression("!", true, new MemberExpression(new Identifier("Number"), new CallExpression(new Identifier("isInteger"), List(identifier)), false))
    else
      toInstanceOf(identifier, typeName)
  }
  
  def toArrowFunction(md: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) = {
    val statements = toBlockStatement(md.getBody).body.toList
    val patterns = toIdentifiers(md.parameters)
    val let = new VariableDeclaration(
      List(new VariableDeclarator(new ArrayPattern(patterns), new Identifier("args"))),
      "let"
    )
    new ArrowFunctionExpression(
      List(new RestElement(new Identifier("args"))),
      new BlockStatement(let +: statements),
      false
    )
  }
  
  def parseSameArgLength(declarations: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration) : Statement = {
    def convertTypeOverloads(mds: Iterable[dom.MethodDeclaration]) : Statement = {
      if (mds.size > 0) {
        val es = mds.head.parameters.collect({ case x: dom.SingleVariableDeclaration => x }).zipWithIndex map { case (x, i) => varToBinaryExpression(x, i) }
        val test = if (es.size == 3) 
          new LogicalExpression("&&", es(2), new LogicalExpression("&&", es(0), es(1)))
        else if (es.size == 2) 
          new LogicalExpression("&&", es(0), es(1))
        else 
          es(0)
        val args = List(new SpreadElement(new Identifier("args")))
        val call = new CallExpression(toArrowFunction(mds.head), args)
        val consequent = new BlockStatement(List(new ReturnStatement(call)))
        new IfStatement(test, consequent, convertTypeOverloads(mds.tail))
      } else null
    }
    
    if (declarations.size > 1) {
      val body = new BlockStatement(List(convertTypeOverloads(declarations)))  
      convertTypeOverloads(declarations)
    } else {
       val args = List(new SpreadElement(new Identifier("args")))
       new ReturnStatement(new CallExpression(toArrowFunction(declarations.head), args))
    }
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
  
  def fromFieldDeclarationStatic(declaration: dom.FieldDeclaration)(implicit td: dom.TypeDeclaration) = { 
    declaration.fragments collect { 
      case field: dom.VariableDeclarationFragment if dom.Modifier.isStatic(declaration.getModifiers) => {
        if (field.resolveBinding == null) throw new RuntimeException("Cannot resolve binding of VariableDeclarationFragment when parsing " + field + " with parent " + field.getParent)    
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
        new Identifier(field.resolveBinding.getDeclaringClass.getName),
        new Identifier(field.getName.getIdentifier), false),
        toExpression(field.getInitializer)))
      }
    }
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