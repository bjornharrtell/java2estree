package org.wololo.java2estree

import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import org.wololo.estree._
import org.eclipse.jdt.core.dom
import com.typesafe.scalalogging.LazyLogging

object ExpressionConversions extends LazyLogging {
  def resolveSimpleName(s: dom.SimpleName) = { 
    //println(s)
    if (s.resolveBinding == null) throw new RuntimeException("Cannot resolve binding of SimpleName")
    s.resolveBinding match { 
      case b: dom.IVariableBinding if b.isParameter() =>
        new Identifier(s.getFullyQualifiedName)
      case b: dom.IVariableBinding if !b.isField() =>
        new Identifier(s.getFullyQualifiedName)
      case b: dom.IVariableBinding if !b.isParameter() =>
        val member = if (dom.Modifier.isStatic(b.getModifiers)) 
          new Identifier(b.getDeclaringClass.getName)
        else 
          new ThisExpression()
        new MemberExpression(member, new Identifier(s.getFullyQualifiedName), false)
      case b: dom.ITypeBinding =>
        new Identifier(s.getFullyQualifiedName)
    }
  }
  
  def resolveQualifiedName(q: dom.QualifiedName) = {
    //println(q)
    if (q.getQualifier.resolveBinding == null) throw new RuntimeException("Cannot resolve binding of the Qualifier of a QualifiedName")
    q.getQualifier.resolveBinding match { 
      case b: dom.IVariableBinding =>
        new Identifier(q.getFullyQualifiedName)
      case b: dom.ITypeBinding =>
        new Identifier(q.getFullyQualifiedName)
      case b: dom.IPackageBinding =>
        new Identifier(q.getFullyQualifiedName)
    }
  }
  
  def translateOp(op: String) = op match {
    case "==" => "===" 
    case "!=" => "!=="
    case x => x
  }
  
  def translateToken(token: String) =
    if (token.last == 'L')
      token.substring(0, token.length-1)
    else token
    
  def toBinaryExpression(op: String, l: Buffer[Expression]) : BinaryExpression =
    if (l.length > 2) 
      new BinaryExpression(op, l.head, toBinaryExpression(op, l.tail))
    else
      new BinaryExpression(op, l.head, l.get(1))
  
  def toExpressions(expressions: java.util.List[_])(implicit td: dom.TypeDeclaration) =
    expressions collect { case x: dom.Expression => toExpression(x)}
  
  def toExpression(e: dom.Expression)(implicit td: dom.TypeDeclaration): Expression = e match {
    case nl: dom.NullLiteral => new Literal("null", "null")
    case x: dom.SimpleName => resolveSimpleName(x)
    case x: dom.QualifiedName => resolveQualifiedName(x)
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
      new Literal(x.getType.toString, x.getType.toString)
    case x: dom.ArrayCreation =>
      val elements = if (x.getInitializer == null) List() else toExpressions(x.getInitializer.expressions)
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
      val ops = Buffer(toExpression(x.getLeftOperand), toExpression(x.getRightOperand))
      val exops = ops ++ toExpressions(x.extendedOperands)
      toBinaryExpression(translateOp(x.getOperator.toString), exops)
    case x: dom.InstanceofExpression =>
      val t = x.getRightOperand.resolveBinding.getName
      new BinaryExpression("instanceof", toExpression(x.getLeftOperand), new Literal(t, t))
    case x: dom.CastExpression => 
      // TODO: special case handle cast double/float -> int
      toExpression(x.getExpression)
    case x: dom.ClassInstanceCreation =>
      new NewExpression(
          new Identifier(x.getType.toString), toExpressions(x.arguments))
    case x: dom.FieldAccess =>
      if (x.resolveFieldBinding == null) throw new RuntimeException("Cannot resolve binding of FieldAccess")
      val t = if (dom.Modifier.isStatic(x.resolveFieldBinding.getModifiers))
        new Identifier(td.getName.getIdentifier)
      else
        new ThisExpression()
      new MemberExpression(t, new Identifier(x.getName.getIdentifier), false)
    case x: dom.MethodInvocation =>
      if (x.resolveMethodBinding == null) throw new RuntimeException("Cannot resolve binding of MethodInvocation")
      val t = if (x.getExpression == null && !dom.Modifier.isStatic(x.resolveMethodBinding.getModifiers)) 
        new ThisExpression()
      else if (x.getExpression == null && dom.Modifier.isStatic(x.resolveMethodBinding.getModifiers))
        new Identifier(td.getName.getIdentifier)
      else
        toExpression(x.getExpression)
      val callee = new MemberExpression(t, new Identifier(x.getName.getIdentifier), false)
      new CallExpression(callee, toExpressions(x.arguments))
    case x: dom.SuperFieldAccess =>
      new Literal("super", "super")
    case x: dom.SuperMethodInvocation =>
      val callee = new MemberExpression(new Super(), new Identifier(x.getName.getIdentifier), false)
      new CallExpression(callee, toExpressions(x.arguments))
    case null =>
      new Literal(null, "null")
    /*case x => {
      logger.debug(s"Unexpected expression ($x})")
      new Literal("null", "null")
    }*/
  }
}