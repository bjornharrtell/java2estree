package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import org.eclipse.jdt.core.dom
import compilationunit._
import expression._
import statement._
import scala.collection.mutable.Buffer

object method {
  def toFunctionExpression(x: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) =
    new FunctionExpression(
      fromParameters(x.parameters),
      fromBlock(x.getBody)
    )
  
  def checkInterfaceExpression(e: Expression, typeName: String) : LogicalExpression = {
    val interfaces = new MemberExpression(e, new Identifier("interfaces_"), false)
    
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
  
  def fromOverloadedMethodDeclarations(x: Iterable[dom.MethodDeclaration], returns: Boolean, hasSuper: Boolean = false, overloadedConstructor: Boolean = false)(implicit td: dom.TypeDeclaration) = {
    def fromSameArgLength(declarations: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration): List[Statement] = {
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
          
          val consequent = if (hasSuper && overloadedConstructor) {
            val args = List(new SpreadElement(new Identifier("args")))
            val call = new CallExpression(toArrowFunction(mds.head), args)
            new BlockStatement(List(new ReturnStatement(call)))
          } else {
            // TODO: should be able to work for overloaded constructors too
            val statements = fromBlock(mds.head.getBody).body.toList
            new BlockStatement(argsToLet(mds.head) +: statements)
          }
          new IfStatement(test, consequent, fromTypeOverloads(mds.tail))
        } else null
      }
      
      if (declarations.size > 1) {
        // TODO: Not sure this follows https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.12.2.5
        // TODO: Consider all params
        var sorted = declarations.toList.sortWith { (md1, md2) => {
          val b1 = md1.parameters()(0).asInstanceOf[dom.SingleVariableDeclaration].getType.resolveBinding
          val b2 = md2.parameters()(0).asInstanceOf[dom.SingleVariableDeclaration].getType.resolveBinding
          b1.isSubTypeCompatible(b2)
        } }
        List(fromTypeOverloads(sorted))
      } else {
        if (hasSuper && overloadedConstructor) {
          val args = List(new SpreadElement(new Identifier("args")))
          val call = new CallExpression(toArrowFunction(declarations.head), args)
          if (returns) List(new ReturnStatement(call)) else List(new ExpressionStatement(call))
        } else {
          // TODO: should be able to work for overloaded constructors too
          val statements = fromBlock(declarations.head.getBody).body.toList
          argsToLet(declarations.head) +: statements
        }
      }
    }
    
    
    val cases = x.groupBy({ _.parameters.length }).toList.sortBy(_._1).collect({
      case (argsCount, methods) => (new Literal(argsCount, argsCount.toString), fromSameArgLength(methods))
    })
    
    if (cases.size == 0) {
      List()
    }
    else {
      val test = new BinaryExpression("===", new Identifier("args.length"), cases.head._1)
      val ifStatement = toIf(new IfStatement(test, new BlockStatement(cases.head._2), null), cases.tail.toBuffer)
      
      if (overloadedConstructor) {
        val args = List(new RestElement(new Identifier("args")))
        val overloaded = new ArrowFunctionExpression(args, new BlockStatement(List(ifStatement)), true, false)
        val overloadedConst = new VariableDeclaration(
          List(new VariableDeclarator(new Identifier("overloaded"), overloaded)), "const")
        val overloadedApply = new MemberExpression(new Identifier("overloaded"), new Identifier("apply"), false)
        val overloadedApplyCall = new CallExpression(overloadedApply, List(new ThisExpression, new Identifier("args")))
        val returnStatement = new ReturnStatement(overloadedApplyCall)
        List(overloadedConst, returnStatement)
        /* Fails with 'super' outside of function or class
        val params = List(new RestElement(new Identifier("args")))
        var overloaded = new FunctionDeclaration(new Identifier("overloaded"), params, new BlockStatement(List(switch)))
        val overloadedApply = new MemberExpression(new Identifier("overloaded"), new Identifier("apply"), false)
        val overloadedApplyCall = new CallExpression(overloadedApply, List(new ThisExpression, new Identifier("args")))
        val returnStatement = new ReturnStatement(overloadedApplyCall)
        List(overloaded, returnStatement)
        */
      } else {
        List(ifStatement)
      }
    }    
  }
  
  def toIf(ifs: IfStatement, rest: Buffer[(Literal, List[Statement])]) : IfStatement = {
    if (rest.length == 0) {
      ifs
    } else {
      val test = new BinaryExpression("===", new Identifier("args.length"), rest.head._1)
      val alternate = new IfStatement(test, new BlockStatement(rest.head._2), null)
      new IfStatement(ifs.test, ifs.consequent, toIf(alternate, rest.tail))
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

  def fromMethodDeclarations(x: Iterable[dom.MethodDeclaration], overloadedConstructor: Boolean = false)(implicit td: dom.TypeDeclaration) : MethodDefinition = {
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
            new BlockStatement(fromOverloadedMethodDeclarations(x, true, overloadedConstructor))
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
        val left = new MemberExpression(new Identifier(td.getName.getIdentifier), new Identifier(field.getName.getIdentifier), false)
        val right = toExpression(field.getInitializer)
        new AssignmentExpression("=", left, right)
      }
    }
  }
}