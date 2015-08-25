package org.wololo.java2estree

import scala.collection.JavaConversions._

import org.wololo.estree._
import OperatorConversions._

import com.github.javaparser.{ ast => jp }

import com.typesafe.scalalogging.LazyLogging

object ExpressionConversions extends LazyLogging {
  implicit def expression: PartialFunction[jp.expr.Expression, Expression] = {
    case nl: jp.expr.NullLiteralExpr => new Literal("null", "null")
    case n: jp.expr.NameExpr => new Identifier(n.getName)
    case x: jp.expr.ThisExpr => new ThisExpression()
    case x: jp.expr.AssignExpr =>
      new AssignmentExpression(x.getOperator, x.getTarget, x.getValue)
    case ee: jp.expr.EnclosedExpr => ee.getInner
    case x: jp.expr.BooleanLiteralExpr =>
      new Literal(x.getValue, x.getValue.toString)
    case il: jp.expr.IntegerLiteralExpr =>
      new Literal(il.getValue, il.getValue)
    case il: jp.expr.LongLiteralExpr =>
      new Literal(il.getValue, il.getValue)
    case il: jp.expr.DoubleLiteralExpr =>
      new Literal(il.getValue, il.getValue)
    case il: jp.expr.CharLiteralExpr =>
      new Literal(il.getValue, "\"" + il.getValue + "\"")
    case il: jp.expr.StringLiteralExpr =>
      new Literal(il.getValue, "\"" + il.getValue + "\"")
    case x: jp.expr.ArrayCreationExpr =>
      // TODO: implement
      new Literal("null", "null")
    case x: jp.expr.UnaryExpr =>
      new UnaryExpression(x.getOperator, true, x.getExpr)
    case be: jp.expr.BinaryExpr =>
      new BinaryExpression(be.getOperator, be.getLeft, be.getRight)
    case x: jp.expr.InstanceOfExpr =>
      new BinaryExpression("instanceof", x.getExpr, 
          new Literal(x.getType.toString(), x.getType.toString()))
    case x: jp.expr.CastExpr => x.getExpr
    case x: jp.expr.ObjectCreationExpr =>
      new NewExpression(
          new Identifier(x.getType.getName), if (x.getArgs == null) List() else x.getArgs map expression)
    case x: jp.expr.FieldAccessExpr =>
      new MemberExpression(
          if (x.getScope == null) new ThisExpression else x.getScope,
          new Identifier(x.getField), false)
    case x: jp.expr.MethodCallExpr =>
      new CallExpression(new MemberExpression(
          if (x.getScope == null) new ThisExpression else x.getScope,
          new Identifier(x.getName), false), 
          if (x.getArgs == null) List() else x.getArgs map expression)
    case x: jp.expr.SuperExpr =>
      new Literal("super", "super")
    case null =>
      new Literal("null", "null")
    /*case x => {
      logger.debug(s"Unexpected expression ($x})")
      new Literal("null", "null")
    }*/
  }
}