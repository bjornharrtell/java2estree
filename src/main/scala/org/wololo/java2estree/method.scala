package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import org.eclipse.jdt.core.dom
import compilationunit._
import expression._
import statement._

object method {
  def toFunctionExpression(x: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) =
    new FunctionExpression(
      fromParameters(x.parameters),
      fromBlock(x.getBody)
    )
  
  def checkInterfaceExpression(x: MemberExpression, typeName: String) : LogicalExpression = {
    val interfaces = new MemberExpression(x, new Identifier("interfaces_"), false)
    
    val indexOf = new MemberExpression(interfaces, new Identifier("indexOf"), false)
    val indexOfCall = new CallExpression(indexOf, List(new Identifier(typeName)))
    
    val interfaceCheck = new BinaryExpression(">", indexOfCall, new Literal(-1, "-1"))
    new LogicalExpression("&&", interfaces, interfaceCheck)
  } 
  
  def varToBinaryExpression(x: dom.SingleVariableDeclaration, i: Int) = {
    val identifier = new MemberExpression(new Identifier("args"), new Literal(i, i.toString), true)
    val binding = x.getType.resolveBinding
    val isInterface = binding.isInterface
    val typeName = binding.getName
    if (x.getType.isArrayType())
      toInstanceOf(identifier, "Array")
    else if (typeName == "boolean")
      new BinaryExpression("===", new UnaryExpression("typeof", true, identifier), new Literal("boolean", "\"boolean\""))
    else if (typeName == "int")
      new MemberExpression(new Identifier("Number"), new CallExpression(new Identifier("isInteger"), List(identifier)), false)
    else if (typeName == "double")
      new UnaryExpression("!", true, new MemberExpression(new Identifier("Number"), new CallExpression(new Identifier("isInteger"), List(identifier)), false))
    else if (isInterface)
      checkInterfaceExpression(identifier, typeName)
    else
      toInstanceOf(identifier, typeName)
  }
  
  def toArrowFunction(md: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) = {
    val statements = fromBlock(md.getBody).body.toList
    val patterns = fromParameters(md.parameters)
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
  
  def fromOverloadedMethodDeclarations(x: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration) = {
    def fromSameArgLength(declarations: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration): Statement = {
      def fromTypeOverloads(mds: Iterable[dom.MethodDeclaration]) : Statement = {
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
          new IfStatement(test, consequent, fromTypeOverloads(mds.tail))
        } else null
      }
      
      if (declarations.size > 1) {
        fromTypeOverloads(declarations)
      } else {
        val args = List(new SpreadElement(new Identifier("args")))
        new ReturnStatement(new CallExpression(toArrowFunction(declarations.head), args))
      }
    }
    
    
    val cases = x.groupBy { _.parameters.length }.collect {
      case (k, v) => 
        new SwitchCase(new Literal(k, k.toString), List(fromSameArgLength(v)))
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
      new FunctionExpression(fromParameters(x.parameters),
          fromBlock(x.getBody)),
      "method",
      false,
      dom.Modifier.isStatic(x.getModifiers)
    )
  
  def fromMethodDeclarationOverloads(x: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration) = {
    new MethodDefinition(
      new Identifier(x.head.getName.getIdentifier),
      new FunctionExpression(
          List(new RestElement(new Identifier("args"))),
          fromOverloadedMethodDeclarations(x)),
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