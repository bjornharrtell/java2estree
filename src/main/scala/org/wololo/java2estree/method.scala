package org.wololo.java2estree

import org.wololo.estree._
import scala.jdk.CollectionConverters._
import org.eclipse.jdt.core.dom
import compilationunit._
import expression._
import statement._
import scala.collection.mutable.Buffer
import org.eclipse.jdt.core.dom.TypeDeclaration
import org.eclipse.jdt.core.dom.MethodDeclaration

object method:
  def toFunctionExpression(x: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration) =
    new FunctionExpression(
      fromParameters(x.parameters),
      fromBlock(x.getBody)
    )

  def checkInterfaceExpression(e: Expression, typeName: String): CallExpression =
    new CallExpression(new Identifier("hasInterface"), List(e, new Identifier(typeName)))

  def varToBinaryExpression(binding: dom.ITypeBinding, i: Int) =
    val identifier = new MemberExpression(new Identifier("arguments"), new Literal(i, i.toString), true)
    val isInterface = binding.isInterface
    // remove parameterized type stuff
    val typeName = binding.getName.replaceAll("<.*>", "")
    if (binding.isArray)
      toInstanceOf(identifier, "Array")
    else if (typeName == "boolean")
      new BinaryExpression("===", new UnaryExpression("typeof", true, identifier), new Literal("boolean", "\"boolean\""))
    else if (typeName == "int")
      new MemberExpression("Number", new CallExpression(new Identifier("isInteger"), List(identifier)))
    else if (typeName == "double")
      new BinaryExpression("===", new UnaryExpression("typeof", true, identifier), new Literal("number", "\"number\""))
    else if (isInterface)
      checkInterfaceExpression(identifier, typeName)
    else
      toInstanceOf(identifier, typeName)

  def argsToLet(patterns: List[Identifier]): VariableDeclaration =
    val declarators = patterns.zipWithIndex.map({
      case (e, i) =>
        new VariableDeclarator(e, new MemberExpression(new Identifier("arguments"), new Literal(i, i.toString()), true))
    })
    new VariableDeclaration(declarators, "let")

  def fromOverloadedMethodDeclarations(x: Iterable[dom.MethodDeclaration], returns: Boolean)(implicit td: dom.TypeDeclaration) =
    def fromSameArgLength(declarations: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration): List[Statement] =
      def fromTypeOverloads(mds: Iterable[dom.MethodDeclaration]): Statement =
        if (mds.size > 0)
          val es = mds.map(d => d.resolveBinding).head.getParameterTypes.zipWithIndex map { case (x, i) => varToBinaryExpression(x, i) }
          // TODO: make this recursive
          val test = if (es.size == 6)
            new LogicalExpression("&&", es(5), new LogicalExpression("&&", es(4), new LogicalExpression("&&", es(3), new LogicalExpression("&&", es(2), new LogicalExpression("&&", es(0), es(1))))))
          else if (es.size == 5)
            new LogicalExpression("&&", es(4), new LogicalExpression("&&", es(3), new LogicalExpression("&&", es(2), new LogicalExpression("&&", es(0), es(1)))))
          else if (es.size == 4)
            new LogicalExpression("&&", es(3), new LogicalExpression("&&", es(2), new LogicalExpression("&&", es(0), es(1))))
          else if (es.size == 3)
            new LogicalExpression("&&", es(2), new LogicalExpression("&&", es(0), es(1)))
          else if (es.size == 2)
            new LogicalExpression("&&", es(0), es(1))
          else
            es(0)

          val bodyStatements = fromBlock(mds.head.getBody).body.toList
          var patterns = fromParameters(mds.head.parameters).toList
          val statements = if (patterns.length > 0)
            argsToLet(patterns) +: bodyStatements
          else bodyStatements
          val consequent = new BlockStatement(statements)
          new IfStatement(test, consequent, fromTypeOverloads(mds.tail))
        else null
      
      if (declarations.size > 1)
                
        // TODO: Not sure this follows https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.12.2.5
        // TODO: Consider all params
        var sorted = declarations.toList.sortWith { (md1, md2) => {
            val b1 = md1.parameters().asScala(0).asInstanceOf[dom.SingleVariableDeclaration].getType.resolveBinding
            val b2 = md2.parameters().asScala(0).asInstanceOf[dom.SingleVariableDeclaration].getType.resolveBinding
            b1.isSubTypeCompatible(b2)
          }
        }
        List(fromTypeOverloads(sorted))
      else
        val bodyStatements = fromBlock(declarations.head.getBody).body.toList
        var patterns = fromParameters(declarations.head.parameters).toList
        if (patterns.length > 0) argsToLet(patterns) +: bodyStatements
        else bodyStatements

    val cases = x.groupBy({ _.parameters.asScala.length }).toList.sortBy(_._1).collect({
      case (argsCount, methods) => (new Literal(argsCount, argsCount.toString), fromSameArgLength(methods))
    })
    
    if (cases.size == 0) List()
    else if (cases.size == 1) cases.head._2
    else
      val test = new BinaryExpression("===", new Identifier("arguments.length"), cases.head._1)
      val ifStatement = toIf(new IfStatement(test, new BlockStatement(cases.head._2), null), cases.tail.toBuffer)
      List(ifStatement)

  def toIf(ifs: IfStatement, rest: Buffer[(Literal, List[Statement])]): IfStatement =
    if (rest.length == 0)
      ifs
    else
      val test = new BinaryExpression("===", new Identifier("arguments.length"), rest.head._1)
      val alternate = new IfStatement(test, new BlockStatement(rest.head._2), null)
      new IfStatement(ifs.test, ifs.consequent, toIf(alternate, rest.tail))

  def specificMethodConditional(m: dom.MethodDeclaration)(implicit td: dom.TypeDeclaration): IfStatement =
    val argsLength = new MemberExpression("arguments", "length")
    //var test = new BinaryExpression("===", argsLength, new Literal(m.parameters.size(), m.parameters.size().toString()))

    val est = m.resolveBinding.getParameterTypes.zipWithIndex map { case (x, i) => varToBinaryExpression(x, i) }
    
    val es = new BinaryExpression("===", argsLength, new Literal(m.parameters.size(), m.parameters.size().toString())) +: est
    
    val test = if (es.size == 3)
      new LogicalExpression("&&", es(0), new LogicalExpression("&&", es(2), es(1)))
    else if (es.size == 2)
      new LogicalExpression("&&", es(0), es(1))
    else
      es(0)
    
    val bodyStatements = fromBlock(m.getBody).body.toList
    var patterns = fromParameters(m.parameters).toList
    val statements = if (patterns.length > 0)
      argsToLet(patterns) +: bodyStatements
    else bodyStatements

    val consequent = new BlockStatement(statements)
    
    val alternate = if (td.getSuperclassType != null)
      var apply = new MemberExpression(m.getName.getIdentifier, "apply")
      val call = new CallExpression(apply, List(new ThisExpression, new Identifier("arguments")))
      val superCall = new MemberExpression(new Super(), call)
      new ReturnStatement(superCall)
    else null
    new IfStatement(test, consequent, alternate)

  /**
   * Check superClass for overloads that are not overridden by m
   */
  def hasSuperOverloads(m: dom.MethodDeclaration, superClass: dom.ITypeBinding): Boolean =
    val binding = m.resolveBinding
    if (superClass != null)
      if (superClass.getDeclaredMethods.exists(x => x.getName == m.getName.getIdentifier && !binding.overrides(x)))
        true
      else
        hasSuperOverloads(m, superClass.getSuperclass)
    else false
  
  def findSuperOverloads(m: dom.MethodDeclaration, superClass: dom.ITypeBinding, mbs: List[dom.IMethodBinding]): Iterable[dom.IMethodBinding] =
    val binding = m.resolveBinding
    if (superClass != null)
      val methodBindings = superClass.getDeclaredMethods.filter(x => x.getName == m.getName.getIdentifier && !binding.overrides(x))
      findSuperOverloads(m, superClass.getSuperclass, mbs ++ methodBindings)
    else
      mbs

  def fromMethodDeclarations(x: Iterable[dom.MethodDeclaration])(implicit td: dom.TypeDeclaration): FunctionDeclaration =
    // check if single method has non overrided overload
    val binding = x.head.resolveBinding
    val superClass = binding.getDeclaringClass.getSuperclass
    val superOverloads = hasSuperOverloads(x.head, superClass)
    
    if (x.size == 1)
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
    else
      new FunctionDeclaration(
        new Identifier(x.head.getName.getIdentifier),
        List(),
        new BlockStatement(fromOverloadedMethodDeclarations(x, true)),
        false
      )

  def fromFieldDeclarationMember(declaration: dom.FieldDeclaration)(implicit td: dom.TypeDeclaration) =
    val isInterface = td.isInterface()
    val isStatic = dom.Modifier.isStatic(declaration.getModifiers)
    val isProtected = dom.Modifier.isProtected(declaration.getModifiers)
    val isPrivate = dom.Modifier.isPrivate(declaration.getModifiers)
    val prefix = if (isPrivate || isProtected) "_" else ""
    declaration.fragments.asScala collect {
      case field: dom.VariableDeclarationFragment if (!isStatic && !isInterface) =>
        new ExpressionStatement(new AssignmentExpression("=", new MemberExpression(
            new ThisExpression(), prefix + field.getName.getIdentifier
          ),
          toExpression(field.getInitializer)
        ))
    }

  def fromFieldDeclarationStatic(declaration: dom.FieldDeclaration)(implicit td: dom.TypeDeclaration) =
    val isInterface = td.isInterface()
    var isStatic = dom.Modifier.isStatic(declaration.getModifiers)
    declaration.fragments.asScala collect {
      case field: dom.VariableDeclarationFragment if (isStatic || isInterface) && field.getName.getIdentifier != "serialVersionUID" => {
        if (field.resolveBinding == null) throw new RuntimeException("Cannot resolve binding of VariableDeclarationFragment when parsing " + field + " with parent " + field.getParent)
        val typeName = td.getName.getIdentifier
        var fieldName = field.getName.getIdentifier
        val left = new MemberExpression(typeName, fieldName)
        val right = toExpression(field.getInitializer)
        new AssignmentExpression("=", left, right)
      }
    }
