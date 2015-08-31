package org.wololo.java2estree

import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import org.wololo.estree._
import org.eclipse.jdt.core.{ dom => jp }
import com.typesafe.scalalogging.LazyLogging
import org.eclipse.jdt.core.dom.TypeDeclaration
import org.eclipse.jdt.core.dom.Modifier

object ExpressionConversions extends LazyLogging {
  def resolveSimpleName(s: jp.SimpleName) = s.resolveBinding match { 
    case b: jp.IVariableBinding if b.isParameter() =>
      new Identifier(s.getFullyQualifiedName)
    case b: jp.IVariableBinding if !b.isField() =>
       new Identifier(s.getFullyQualifiedName)
    case b: jp.IVariableBinding if !b.isParameter() =>
      new MemberExpression(new Identifier("this"), new Identifier(s.getFullyQualifiedName), false)
  }
  
  def resolveQualifiedName(q: jp.QualifiedName) = {
    if (q.getQualifier.resolveBinding == null) throw new RuntimeException("Cannot resolve binding")
    q.getQualifier.resolveBinding match { 
      case b: jp.IVariableBinding =>
        new Identifier(q.getFullyQualifiedName)
      case b: jp.ITypeBinding =>
        new Identifier(q.getFullyQualifiedName)
    }
  }
  
  def toOp(op: String) = op match {
    case "==" => "===" 
    case "!=" => "!=="
    case x => x
  }
  
  def toBinaryExpression(op: String, l: Buffer[Expression]) : BinaryExpression =
    if (l.length > 2) 
      new BinaryExpression(op, l.head, toBinaryExpression(op, l.tail))
    else
      new BinaryExpression(op, l.head, l.get(1))
  
  def toExpression(e: jp.Expression)(implicit td: jp.TypeDeclaration): Expression = e match {
    case nl: jp.NullLiteral => new Literal("null", "null")
    case x: jp.SimpleName => resolveSimpleName(x)
    case x: jp.QualifiedName => resolveQualifiedName(x)
    case x: jp.ThisExpression => new ThisExpression()
    case x: jp.Assignment =>
      new AssignmentExpression(x.getOperator.toString,
          toExpression(x.getLeftHandSide), toExpression(x.getRightHandSide))
    case x: jp.ParenthesizedExpression => toExpression(x.getExpression)
    case x: jp.BooleanLiteral =>
      new Literal(x.booleanValue, x.booleanValue.toString)
    case x: jp.NumberLiteral =>
      new Literal(x.getToken, x.getToken)
    case x: jp.CharacterLiteral =>
      new Literal(x.getEscapedValue, x.getEscapedValue)
    case x: jp.StringLiteral =>
      new Literal(x.getLiteralValue, x.getEscapedValue)
    case x: jp.TypeLiteral =>
      new Literal(x.getType.toString, x.getType.toString)
    case x: jp.ArrayCreation =>
      // TODO: implement
      new Literal("null", "null")
    case x: jp.PrefixExpression =>
      new UnaryExpression(x.getOperator.toString, true, toExpression(x.getOperand))
    case x: jp.PostfixExpression =>
      new UnaryExpression(x.getOperator.toString, false, toExpression(x.getOperand))
    case x: jp.InfixExpression =>
      val ops = Buffer(toExpression(x.getLeftOperand), toExpression(x.getRightOperand))
      val exops = ops ++ (x.extendedOperands map { a => toExpression(a.asInstanceOf[jp.Expression]) })
      toBinaryExpression(toOp(x.getOperator.toString), exops)
    case x: jp.InstanceofExpression =>
      new BinaryExpression("instanceof", toExpression(x.getLeftOperand),
          new Literal(x.getRightOperand.toString, x.getRightOperand.toString))
    case x: jp.CastExpression => toExpression(x.getExpression)
    case x: jp.ClassInstanceCreation =>
      new NewExpression(
          new Identifier(x.getType.toString), x.arguments map { x => toExpression(x.asInstanceOf[jp.Expression]) })
    case x: jp.FieldAccess =>
      new MemberExpression(
          new ThisExpression(),
          new Identifier(x.getName.getIdentifier), false)
    case x: jp.MethodInvocation =>
      if (x.resolveMethodBinding == null) throw new RuntimeException("Cannot resolve binding")
      new CallExpression(new MemberExpression(
          if (td.resolveBinding.getKey == x.resolveMethodBinding.getDeclaringClass.getKey &&
              !Modifier.isStatic(x.resolveMethodBinding.getModifiers))
            new ThisExpression
          else 
            new Identifier(x.resolveMethodBinding.getDeclaringClass.getName),
          new Identifier(x.getName.getIdentifier), false),
          x.arguments map { x => toExpression(x.asInstanceOf[jp.Expression]) }
      )
    case x: jp.SuperFieldAccess =>
      new Literal("super", "super")
    case x: jp.SuperMethodInvocation =>
      new CallExpression(new MemberExpression(
          new Identifier("super"),
          new Identifier(x.getName.getIdentifier), false),
          x.arguments map { x => toExpression(x.asInstanceOf[jp.Expression]) }
      )
    case null =>
      new Literal("null", "null")
    /*case x => {
      logger.debug(s"Unexpected expression ($x})")
      new Literal("null", "null")
    }*/
  }
}