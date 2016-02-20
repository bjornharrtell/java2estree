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
  
  def checkInterfaceExpression(e: Expression, typeName: String) : CallExpression = {
    new CallExpression(new Identifier("hasInterface"), List(e, new Identifier(typeName)))
  } 
  
  def varToBinaryExpression(x: dom.SingleVariableDeclaration, i: Int) = {
    val identifier = new MemberExpression(new Identifier("arguments"), new Literal(i, i.toString), true)
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
  def argsToLet(patterns: List[Identifier]) : VariableDeclaration = {
    val declarators = patterns.zipWithIndex.map({ case (e, i) => 
      new VariableDeclarator(e, new MemberExpression(new Identifier("arguments"), new Literal(i, i.toString()), true))
    })
    
    new VariableDeclaration(
      declarators,
      "let"
    )
  }
  
  def toArrowFunction(md: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) = {
    val bodyStatements = fromBlock(md.getBody).body.toList
    var patterns = fromParameters(md.parameters).toList
    val statements = if (patterns.length > 0) 
      argsToLet(patterns) +: bodyStatements
    else bodyStatements
      
    new ArrowFunctionExpression(
      List(),
      new BlockStatement(statements),
      false
    )
  }
  
  def fromOverloadedMethodDeclarations(x: Iterable[dom.MethodDeclaration], returns: Boolean, hasSuper: Boolean = false)(implicit td: dom.TypeDeclaration) = {
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
          
          val consequent = if (hasSuper) {
            val args = List(new Identifier("arguments"))
            val call = new CallExpression(toArrowFunction(mds.head), args)
            new BlockStatement(List(new ReturnStatement(call)))
          } else {
            val bodyStatements = fromBlock(mds.head.getBody).body.toList
            var patterns = fromParameters(mds.head.parameters).toList
            val statements = if (patterns.length > 0) 
              argsToLet(patterns) +: bodyStatements
            else bodyStatements
            new BlockStatement(statements)
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
        val bodyStatements = fromBlock(declarations.head.getBody).body.toList
        var patterns = fromParameters(declarations.head.parameters).toList
        if (patterns.length > 0) 
          argsToLet(patterns) +: bodyStatements
        else bodyStatements
      }
    }
    
    
    val cases = x.groupBy({ _.parameters.length }).toList.sortBy(_._1).collect({
      case (argsCount, methods) => (new Literal(argsCount, argsCount.toString), fromSameArgLength(methods))
    })
    
    if (cases.size == 0) {
      List()
    }
    else {
      val test = new BinaryExpression("===", new Identifier("arguments.length"), cases.head._1)
      val ifStatement = toIf(new IfStatement(test, new BlockStatement(cases.head._2), null), cases.tail.toBuffer)
      List(ifStatement)
    }    
  }
  
  def toIf(ifs: IfStatement, rest: Buffer[(Literal, List[Statement])]) : IfStatement = {
    if (rest.length == 0) {
      ifs
    } else {
      val test = new BinaryExpression("===", new Identifier("arguments.length"), rest.head._1)
      val alternate = new IfStatement(test, new BlockStatement(rest.head._2), null)
      new IfStatement(ifs.test, ifs.consequent, toIf(alternate, rest.tail))
    }
  }
  
  def specificMethodConditional(m: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration): IfStatement = {
    val argsLength = new MemberExpression("arguments", "length")
    val test = new BinaryExpression("===", argsLength, new Literal(m.parameters.size(), m.parameters.size().toString()))
    
    val bodyStatements = fromBlock2(m.getBody).toList
    var patterns = fromParameters(m.parameters).toList
    val statements = if (patterns.length > 0) 
      argsToLet(patterns) +: bodyStatements
    else bodyStatements
    
    val consequent = new BlockStatement(statements)
    var apply = new MemberExpression(m.getName.getIdentifier, "apply")
    val call = new CallExpression(apply, List(new ThisExpression, new Identifier("arguments")))
    val alternate = if (td.getSuperclassType != null) {
      val superClass = td.getSuperclassType.asInstanceOf[dom.SimpleType].getName.getFullyQualifiedName
      val superCall = new MemberExpression(new MemberExpression(superClass, "prototype"), call)
      new ReturnStatement(superCall)
    } else null
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

  def fromMethodDeclarations(x: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration) : FunctionDeclaration = {
    // check if single method has non overrided overload
    val binding = x.head.resolveBinding
    val superClass = binding.getDeclaringClass.getSuperclass
    val superOverloads = hasSuperOverloads(x.head, superClass)
    
    if (x.size == 1) {
      val params = if (superOverloads)
        List()
      else
        fromParameters(x.head.parameters)
        
      val block = if (superOverloads)
        new BlockStatement(List(specificMethodConditional(x.head)))
      else
        fromBlock(x.head.getBody)

      new FunctionDeclaration(
        new Identifier(x.head.getName.getIdentifier),
        params,
        block,
        false
      )
    } else {
      new FunctionDeclaration(
        new Identifier(x.head.getName.getIdentifier),
        List(),
        new BlockStatement(fromOverloadedMethodDeclarations(x, true)),
        false
      )
    }
  }
  
  def fromFieldDeclarationMember(declaration: dom.FieldDeclaration)(implicit td: dom.TypeDeclaration) = 
    declaration.fragments collect {
      case field: dom.VariableDeclarationFragment if !dom.Modifier.isStatic(declaration.getModifiers) =>
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
        new ThisExpression(), field.getName.getIdentifier),
        toExpression(field.getInitializer)))
  }
  
  def fromFieldDeclarationStatic(declaration: dom.FieldDeclaration)(implicit td: dom.TypeDeclaration) = { 
    declaration.fragments collect { 
      case field: dom.VariableDeclarationFragment if dom.Modifier.isStatic(declaration.getModifiers) => {
        if (field.resolveBinding == null) throw new RuntimeException("Cannot resolve binding of VariableDeclarationFragment when parsing " + field + " with parent " + field.getParent)    
        val left = new MemberExpression(td.getName.getIdentifier, field.getName.getIdentifier)
        val right = toExpression(field.getInitializer)
        new AssignmentExpression("=", left, right)
      }
    }
  }
}