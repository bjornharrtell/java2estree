package org.wololo.java2estree

import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import org.wololo.estree._
import org.eclipse.jdt.core.dom
import com.typesafe.scalalogging.LazyLogging
import method._

object expression extends LazyLogging {
  val trunc = new MemberExpression("Math", "trunc")
  def truncCall(e: Expression) = new CallExpression(trunc, List(e))

  def resolve(name: dom.Name) = if (name.isSimpleName())
    resolveSimpleName(name.asInstanceOf[dom.SimpleName])
  else
    resolveQualifiedName(name.asInstanceOf[dom.QualifiedName])

  def resolveSimpleName(s: dom.SimpleName) = {
    if (s.resolveBinding == null) throw new RuntimeException("Cannot resolve binding of SimpleName when parsing " + s + " with parent " + s.getParent)
    val b = s.resolveBinding
    val isPrivate = dom.Modifier.isPrivate(b.getModifiers)
    val isProtected = dom.Modifier.isProtected(b.getModifiers)
    val isStatic = dom.Modifier.isStatic(b.getModifiers)
    val name = s.getFullyQualifiedName
    val identifier = new Identifier(name)
    b match {
      case b: dom.IVariableBinding if b.isParameter() => identifier
      case b: dom.IVariableBinding if !b.isField() => identifier
      case b: dom.IVariableBinding if !b.isParameter() =>
        val member = if (dom.Modifier.isStatic(b.getModifiers))
          new Identifier(b.getDeclaringClass.getName)
        else
          new ThisExpression()
        var prefix = if ((isPrivate || isProtected) && !isStatic) "_" else ""
        new MemberExpression(member, prefix + s.getFullyQualifiedName)
      case b: dom.ITypeBinding => identifier
      case b: dom.IPackageBinding => identifier
    }
  }

  def resolveQualifiedName(q: dom.QualifiedName): Expression = {
    if (q.getQualifier.resolveBinding == null) throw new RuntimeException("Cannot resolve binding of the Qualifier of a QualifiedName when parsing " + q + " with parent " + q.getParent)
    val b = q.getQualifier.resolveBinding
    val b2 = q.resolveBinding
    val isPrivate = dom.Modifier.isPrivate(b2.getModifiers)
    val isProtected = dom.Modifier.isProtected(b2.getModifiers)
    val isStatic = dom.Modifier.isStatic(b2.getModifiers)
    var prefix = if ((isPrivate || isProtected) && !isStatic) "_" else ""
    b match {
      case b: dom.IVariableBinding =>
        new MemberExpression(resolve(q.getQualifier), prefix + q.getName.getIdentifier)
      case b: dom.ITypeBinding =>
        new MemberExpression(resolve(q.getQualifier), q.getName.getIdentifier)
      case b: dom.IPackageBinding =>
        new MemberExpression(resolve(q.getQualifier), q.getName.getIdentifier)
    }
  }

  def translateOp(op: String) = op match {
    case "==" => "==="
    case "!=" => "!=="
    case x    => x
  }

  def translateToken(token: String) =
    if (token.last == 'L')
      token.substring(0, token.length - 1)
    else if (token.last == 'D')
      token.substring(0, token.length - 1)
    else if (token.last == 'd')
      token.substring(0, token.length - 1)
    else token

  /**
   * Convert op and expressions from a dom.InfixExpression and truncate if needed.
   */
  def toBinaryExpression(op: String, left: Expression, rest: Buffer[dom.Expression], shouldTrunc: Boolean)(implicit td: dom.TypeDeclaration): Expression = {

    def be = new BinaryExpression(op, left, toExpression(rest.get(0)))
    if (rest.length == 0)
      left
    else if (rest.length == 1)
      if (shouldTrunc) truncCall(be) else be
    else if (shouldTrunc) toBinaryExpression(op, truncCall(be), rest.tail, shouldTrunc)
    else toBinaryExpression(op, be, rest.tail, shouldTrunc)
  }

  def toInstanceOf(e: Expression, typeName: String) = {
    if (typeName == "String" || typeName == "char") {
      val typeof = new UnaryExpression("typeof", true, e)
      new BinaryExpression("===", typeof, new Literal("string", "\"string\""))
    } else {
      new BinaryExpression("instanceof", e, new Identifier(typeName))
    }
  }

  def toExpressions(expressions: java.util.List[_])(implicit td: dom.TypeDeclaration) =
    expressions collect { case x: dom.Expression => toExpression(x) }

  def create2DArrayExpression(x: Expression, y: Expression): CallExpression = {
    val xCall = new CallExpression(new Identifier("Array"), List(x))
    val yCall = new CallExpression(new Identifier("Array"), List(y))
    val yArrow = new ArrowFunctionExpression(List(), yCall, false, true)

    val innerMember = new MemberExpression(xCall, "fill")
    val innerCall = new CallExpression(innerMember, List())
    val outerMember = new MemberExpression(innerCall, "map")
    new CallExpression(outerMember, List(yArrow))
  }

  def toExpression(e: dom.Expression)(implicit td: dom.TypeDeclaration): Expression = {
    e match {
      case nl: dom.NullLiteral   => new Literal(null, "null")
      case x: dom.SimpleName     => resolveSimpleName(x)
      case x: dom.QualifiedName  => resolveQualifiedName(x)
      case x: dom.ThisExpression => new ThisExpression()
      case x: dom.Assignment =>
        new AssignmentExpression(x.getOperator.toString,
          toExpression(x.getLeftHandSide), toExpression(x.getRightHandSide))
      case x: dom.ParenthesizedExpression => toExpression(x.getExpression)
      case x: dom.BooleanLiteral =>
        new Literal(x.booleanValue, x.booleanValue.toString)
      case x: dom.NumberLiteral =>
        val token = translateToken(x.getToken)
        new Literal(token, token)
      case x: dom.CharacterLiteral =>
        new Literal(x.getEscapedValue, x.getEscapedValue)
      case x: dom.StringLiteral =>
        new Literal(x.getLiteralValue, x.getEscapedValue)
      case x: dom.TypeLiteral =>
        new Identifier(x.getType.toString)
      case x: dom.ArrayCreation =>
        if (x.dimensions().length == 1) {
          val r = toExpression(x.dimensions().get(0).asInstanceOf[dom.Expression])
          val fill = new MemberExpression(new NewExpression(new Identifier("Array"), List(r)), "fill")
          new CallExpression(fill, List(new Literal(null, "null")))
        } else if (x.dimensions().length > 1) {
          val r = toExpression(x.dimensions().get(0).asInstanceOf[dom.Expression])
          val c = toExpression(x.dimensions().get(1).asInstanceOf[dom.Expression])
          create2DArrayExpression(r, c)
        } else if (x.getInitializer != null)
          new ArrayExpression(toExpressions(x.getInitializer.expressions))
        else
          new ArrayExpression(List())
      case x: dom.ArrayInitializer =>
        val elements = toExpressions(x.expressions)
        new ArrayExpression(elements)
      case x: dom.ArrayAccess =>
        new MemberExpression(toExpression(x.getArray), toExpression(x.getIndex), true)
      case x: dom.PrefixExpression =>
        new UnaryExpression(x.getOperator.toString, true, toExpression(x.getOperand))
      case x: dom.PostfixExpression =>
        new UnaryExpression(x.getOperator.toString, false, toExpression(x.getOperand))
      case x: dom.ConditionalExpression =>
        val test = toExpression(x.getExpression)
        val alternate = toExpression(x.getElseExpression)
        val consequent = toExpression(x.getThenExpression)
        new ConditionalExpression(test, alternate, consequent)
      case x: dom.InfixExpression =>
        val op = translateOp(x.getOperator.toString)
        val left = x.getLeftOperand
        val right = x.getRightOperand
        val shouldTrunc = left.resolveTypeBinding().getName == "int" &&
          right.resolveTypeBinding().getName == "int" &&
          op == "/"
        val e = new BinaryExpression(op, toExpression(left), toExpression(right))
        val initial = if (shouldTrunc) truncCall(e) else e
        if (x.extendedOperands.size() > 0)
          toBinaryExpression(op, initial, x.extendedOperands collect { case x: dom.Expression => x }, shouldTrunc)
        else
          initial
      case x: dom.InstanceofExpression =>
        val left = toExpression(x.getLeftOperand)
        val binding = x.getRightOperand.resolveBinding
        val name = x.getRightOperand.resolveBinding.getName
        if (binding.isInterface()) method.checkInterfaceExpression(left, name)
        else toInstanceOf(left, name)
      case x: dom.CastExpression =>
        if (x.getType.toString == "int")
          truncCall(toExpression(x.getExpression))
        else
          toExpression(x.getExpression)
      case x: dom.ClassInstanceCreation =>
        if (x.getAnonymousClassDeclaration != null) {
          val methods = x.getAnonymousClassDeclaration.bodyDeclarations collect { case x: dom.MethodDeclaration => fromMethodDeclarations(List(x)) }
          val properties = methods.map { x => new Property(x.id, new FunctionExpression(x.params, x.body)) }.toList
          val binding = x.getAnonymousClassDeclaration.resolveBinding
          if (binding.getInterfaces.length > 0) {
            var interfaces = binding.getInterfaces.map { x => new Identifier(x.getName) } toList
            var interfacesProperty = compilationunit.createInterfacesProperty(interfaces)
            new ObjectExpression(interfacesProperty +: properties);
          } else {
            new ObjectExpression(properties);
          }
        } else {
          new NewExpression(new Identifier(x.getType.toString), toExpressions(x.arguments))
        }
      case x: dom.FieldAccess =>
        val b = x.resolveFieldBinding
        val isStatic = dom.Modifier.isStatic(b.getModifiers)
        val isPrivate = dom.Modifier.isPrivate(b.getModifiers)
        val isProtected = dom.Modifier.isProtected(b.getModifiers)
        val prefix = if ((isPrivate || isProtected) && !isStatic) "_" else "" 
        x.getExpression match {
          case m: dom.ThisExpression => new MemberExpression(new ThisExpression(), prefix + x.getName.getIdentifier)
          case m: dom.Expression     => new MemberExpression(toExpression(x.getExpression), prefix + x.getName.getIdentifier)
        }
      case x: dom.MethodInvocation =>
        if (x.resolveMethodBinding == null)
          throw new RuntimeException("Cannot resolve binding of MethodInvocation when parsing " + x + " with parent " + x.getParent)
        val binding = x.resolveMethodBinding
        val t = if (x.getExpression == null && !dom.Modifier.isStatic(binding.getModifiers))
          new ThisExpression()
        else if (x.getExpression == null && dom.Modifier.isStatic(binding.getModifiers))
          new Identifier(binding.getDeclaringClass.getName)
        else
          toExpression(x.getExpression)
        val declaringClassName = binding.getDeclaringClass.getName
        val identifier = x.getName.getIdentifier
        // NOTE: special case for String.equals
        if (declaringClassName == "String" && identifier == "equals") {
          // TODO: assumed name of callee
          val left = new MemberExpression(new ThisExpression(), "name")
          val right = toExpressions(x.arguments).head
          return new BinaryExpression("===", left, right)
        }
        val name = if (declaringClassName == "Math" && identifier == "rint") "round" else identifier
        val callee = new MemberExpression(t, name)
        if (binding.getDeclaringClass.getName == "String" && name == "length") callee
        else new CallExpression(callee, toExpressions(x.arguments))
      case x: dom.SuperFieldAccess =>
        throw new RuntimeException("SuperFieldAccess is unsupported")
      case x: dom.SuperMethodInvocation =>
        if (td.getSuperclassType != null) {
          val superClass = td.getSuperclassType.asInstanceOf[dom.SimpleType].getName.getFullyQualifiedName
          val method = new MemberExpression(new MemberExpression(superClass, "prototype"), x.getName.getIdentifier)
          val call = new MemberExpression(method, "call")
          new CallExpression(call, new ThisExpression() +: toExpressions(x.arguments))
        } else new Literal(null, "null") // NOTE: this happens when superclass is out of source tree
      case null =>
        new Literal(null, "null")
      /*case x => {
        logger.debug(s"Unexpected expression ($x})")
        new Literal("null", "null")
      }*/
    }
  }
}