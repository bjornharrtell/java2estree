package org.wololo.java2estree

import scala.collection.JavaConversions._

import org.wololo.estree._

import org.eclipse.jdt.core.{ dom => jp }

import com.typesafe.scalalogging.LazyLogging

object ExpressionConversions extends LazyLogging {
  implicit def expression: PartialFunction[jp.Expression, Expression] = {
    case nl: jp.NullLiteral => new Literal("null", "null")
    case x: jp.Name => x.resolveBinding match {
      case vb: jp.IVariableBinding if vb.isField =>
        new Identifier(x.getFullyQualifiedName)
        // TODO: need to know if this should be:
        // new MemberExpression(new Identifier("this"), new Identifier(x.getFullyQualifiedName), false)
      case vb: jp.IVariableBinding if !vb.isField =>
        new Identifier(x.getFullyQualifiedName)
      case tb: jp.ITypeBinding =>
        new Identifier(tb.getName)
      case null =>
        new Identifier(x.getFullyQualifiedName)
    }
    case x: jp.ThisExpression => new ThisExpression()
    case x: jp.Assignment =>
      new AssignmentExpression(x.getOperator.toString, x.getLeftHandSide, x.getRightHandSide)
    case x: jp.ParenthesizedExpression => x.getExpression
    case x: jp.BooleanLiteral =>
      new Literal(x.booleanValue, x.booleanValue.toString)
    case x: jp.NumberLiteral =>
      new Literal(x.getToken, x.getToken)
    case x: jp.CharacterLiteral =>
      new Literal(x.getEscapedValue, x.getEscapedValue)
    case x: jp.StringLiteral =>
      new Literal(x.getLiteralValue, x.getEscapedValue)
    case x: jp.ArrayCreation =>
      // TODO: implement
      new Literal("null", "null")
    case x: jp.PrefixExpression =>
      new UnaryExpression(x.getOperator.toString, true, x.getOperand)
    case x: jp.PostfixExpression =>
      new UnaryExpression(x.getOperator.toString, false, x.getOperand)
    case x: jp.InfixExpression =>
      new BinaryExpression(x.getOperator.toString, x.getLeftOperand, x.getRightOperand)
    case x: jp.InstanceofExpression =>
      new BinaryExpression("instanceof", x.getLeftOperand, 
          new Literal(x.getRightOperand.toString, x.getRightOperand.toString))
    case x: jp.CastExpression => x.getExpression
    case x: jp.ClassInstanceCreation =>
      new NewExpression(
          new Identifier(x.getType.toString), x.arguments map { x => expression(x.asInstanceOf[jp.Expression]) })
    case x: jp.FieldAccess =>
      new MemberExpression(
          //new Identifier(x.getName.getIdentifier),
          new ThisExpression,
          //if (x.getScope == null) new ThisExpression else x.getScope,
          new Identifier(x.getName.getIdentifier), false)
    case x: jp.MethodInvocation =>
      new CallExpression(new MemberExpression(
          //if (x.getScope == null) new ThisExpression else x.getScope,
          //new Identifier(x.resolveMethodBinding.),
          new ThisExpression(),
          new Identifier(x.getName.getIdentifier), false),
          x.arguments map { x => expression(x.asInstanceOf[jp.Expression]) }
      )
          //List())
          //if (x.getArgs == null) List() else x.getArgs map expression)
    case x: jp.SuperFieldAccess =>
      new Literal("super", "super")
    case x: jp.SuperMethodInvocation =>
      new Literal("super", "super")
    case null =>
      new Literal("null", "null")
    /*case x => {
      logger.debug(s"Unexpected expression ($x})")
      new Literal("null", "null")
    }*/
  }
}