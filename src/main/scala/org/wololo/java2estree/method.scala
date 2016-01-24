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
      new BinaryExpression("===", new UnaryExpression("typeof", true, identifier), new Literal("number", "\"number\""))
    else if (isInterface)
      checkInterfaceExpression(identifier, typeName)
    else
      toInstanceOf(identifier, typeName)
  }
  
  /**
   * Create destructuring statement from args and method params  
   */
  def argsToLet(md: dom.MethodDeclaration) : VariableDeclaration = {
    val patterns = fromParameters(md.parameters)
    new VariableDeclaration(
      List(new VariableDeclarator(new ArrayPattern(patterns), new Identifier("args"))),
      "let"
    )
  }
  
  def toArrowFunction(md: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) = {
    val statements = fromBlock(md.getBody).body.toList
    new ArrowFunctionExpression(
      List(new RestElement(new Identifier("args"))),
      new BlockStatement(argsToLet(md) +: statements),
      false
    )
  }
  
  def fromOverloadedMethodDeclarations(x: Iterable[dom.MethodDeclaration], returns: Boolean = true)(implicit td: dom.TypeDeclaration) = {
    def fromSameArgLength(declarations: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration): Statement = {
      def fromTypeOverloads(mds: Iterable[dom.MethodDeclaration]) : Statement = {
        if (mds.size > 0) {
          val es = mds.head.parameters.collect({ case x: dom.SingleVariableDeclaration => x }).zipWithIndex map { case (x, i) => varToBinaryExpression(x, i) }
          // TODO: make this recursive
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
        // TODO: very naive sort.. need to make sure subclass types are checked before their ancestors
        var sorted = declarations.toList.sortWith { (md1, md2) => {
          if (md1.parameters()(0).asInstanceOf[dom.SingleVariableDeclaration].getType.resolveBinding().getSuperclass == null) false
          else true
        } }
        fromTypeOverloads(sorted)
      } else {
        val args = List(new SpreadElement(new Identifier("args")))
        val call = new CallExpression(toArrowFunction(declarations.head), args)
        if (returns) new ReturnStatement(call) else new ExpressionStatement(call)
      }
    }
    
    
    val cases = x.groupBy({ _.parameters.length }).toList.sortBy(_._1).collect({
      case (argsCount, methods) => new SwitchCase(new Literal(argsCount, argsCount.toString), List(fromSameArgLength(methods)))
    })
    
    if (cases.size == 0) {
      List()
    }
    else {
      var switch = new SwitchStatement(
        new MemberExpression(new Identifier("args"), new Identifier("length"), false),
        cases
      )
      val args = List(new RestElement(new Identifier("args")))
      val overloads = new ArrowFunctionExpression(args, new BlockStatement(List(switch)), true, false)
      val overloadsConst = new VariableDeclaration(
        List(new VariableDeclarator(new Identifier("overloads"), overloads)), "const")
      val overloadsApply = new MemberExpression(new Identifier("overloads"), new Identifier("apply"), false)
      val overloadsApplyCall = new CallExpression(overloadsApply, List(new ThisExpression, new Identifier("args")))
      val returnStatement = new ReturnStatement(overloadsApplyCall)
      List(overloadsConst, returnStatement)
    }    
  }
  
  def specificMethodConditional(m: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration): IfStatement = {
    val argsLength = new MemberExpression(new Identifier("args"), new Identifier("length"), false)
    val test = new BinaryExpression("===", argsLength, new Literal(m.parameters.size(), m.parameters.size().toString()))
    val consequent = new BlockStatement(argsToLet(m) +: fromBlock2(m.getBody).toList)
    val call = new CallExpression(new Identifier(m.getName.getIdentifier), List(new SpreadElement(new Identifier("args"))))
    val alternate = new ReturnStatement(new MemberExpression(new Super(), call, false))
    new IfStatement(test, consequent, alternate)
  }
  
  /**
   * Check superClass for overloads that are not overridden by m
   */
  def hasSuperOverloads(m: dom.MethodDeclaration, superClass: dom.ITypeBinding) : Boolean = {
    val binding = m.resolveBinding
    if (superClass != null)
      if (superClass.getDeclaredMethods.exists(x => x.getName == m.getName.getIdentifier && !binding.overrides(x)))
        true
      else
        hasSuperOverloads(m, superClass.getSuperclass)
    else false
  }

  def fromMethodDeclarations(x: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration) : MethodDefinition = {
    // check if single method has non overrided overload
    val binding = x.head.resolveBinding
    val superClass = binding.getDeclaringClass.getSuperclass
    val superOverloads = hasSuperOverloads(x.head, superClass)
    
    if (x.size == 1) {
      val params = if (superOverloads)
        List(new RestElement(new Identifier("args")))
      else
        fromParameters(x.head.parameters)
        
      val block = if (superOverloads)
        new BlockStatement(List(specificMethodConditional(x.head)))
      else
        fromBlock(x.head.getBody)

      new MethodDefinition(
        new Identifier(x.head.getName.getIdentifier),
        new FunctionExpression(params, block),
        "method",
        false,
        dom.Modifier.isStatic(x.head.getModifiers)
      )
    } else {
      new MethodDefinition(
        new Identifier(x.head.getName.getIdentifier),
        new FunctionExpression(
            List(new RestElement(new Identifier("args"))),
            new BlockStatement(fromOverloadedMethodDeclarations(x))
        ),
        "method",
        false,
        dom.Modifier.isStatic(x.head.getModifiers)
      )
    }
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
        new MethodDefinition(
          new Identifier(field.getName.getIdentifier),
          new FunctionExpression(
              List(),
              new BlockStatement(List(new ReturnStatement(toExpression(field.getInitializer))))
          ),
          "get",
          false,
          true
        )
      }
    }
  }
}