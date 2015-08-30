package org.wololo.java2estree

import scala.collection.JavaConversions._
import org.wololo.estree._
import org.eclipse.jdt.core.{ dom => jp }
import com.typesafe.scalalogging.LazyLogging
import org.eclipse.jdt.core.dom.TypeDeclaration

object ExpressionConversions extends LazyLogging {
  def toExpression(e: jp.Expression)(implicit td: jp.TypeDeclaration): Expression = e match {
    case nl: jp.NullLiteral => new Literal("null", "null")
    case x: jp.Name => x match {
      case s: jp.SimpleName => s.resolveBinding match { 
        case b: jp.IVariableBinding if b.isParameter() =>
          new Identifier(s.getFullyQualifiedName)
        case b: jp.IVariableBinding if !b.isParameter() =>
          new MemberExpression(new Identifier("this"), new Identifier(x.getFullyQualifiedName), false)
      }
      case q: jp.QualifiedName => q.getQualifier.resolveBinding match { 
        case b: jp.IVariableBinding if b.isParameter() =>
          new Identifier(q.getFullyQualifiedName)
        case b: jp.IVariableBinding if !b.isParameter() =>
          new MemberExpression(new Identifier("this"), new Identifier(q.getFullyQualifiedName), false)
        case b: jp.ITypeBinding =>
          new Identifier(q.getFullyQualifiedName)
      }
    }
    case x: jp.ThisExpression => new ThisExpression()
    case x: jp.Assignment =>
      new AssignmentExpression(x.getOperator.toString, toExpression(x.getLeftHandSide), toExpression(x.getRightHandSide))
    case x: jp.ParenthesizedExpression => toExpression(x.getExpression)
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
      new UnaryExpression(x.getOperator.toString, true, toExpression(x.getOperand))
    case x: jp.PostfixExpression =>
      new UnaryExpression(x.getOperator.toString, false, toExpression(x.getOperand))
    case x: jp.InfixExpression =>
      new BinaryExpression(x.getOperator.toString, toExpression(x.getLeftOperand), toExpression(x.getRightOperand))
    case x: jp.InstanceofExpression =>
      new BinaryExpression("instanceof", toExpression(x.getLeftOperand), 
          new Literal(x.getRightOperand.toString, x.getRightOperand.toString))
    case x: jp.CastExpression => toExpression(x.getExpression)
    case x: jp.ClassInstanceCreation =>
      new NewExpression(
          new Identifier(x.getType.toString), x.arguments map { x => toExpression(x.asInstanceOf[jp.Expression]) })
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
          x.arguments map { x => toExpression(x.asInstanceOf[jp.Expression]) }
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