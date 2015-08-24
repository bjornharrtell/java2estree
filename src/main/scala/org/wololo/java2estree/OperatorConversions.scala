package org.wololo.java2estree

import com.github.javaparser.{ ast => jp }

object OperatorConversions {
  implicit def binaryOperator: PartialFunction[jp.expr.BinaryExpr.Operator, String] = {
    case jp.expr.BinaryExpr.Operator.plus => "+"
    case jp.expr.BinaryExpr.Operator.minus => "-"
    case jp.expr.BinaryExpr.Operator.times => "*"
    case jp.expr.BinaryExpr.Operator.divide => "/"
    case jp.expr.BinaryExpr.Operator.remainder => "%"
    case jp.expr.BinaryExpr.Operator.equals => "==="
    case jp.expr.BinaryExpr.Operator.notEquals => "!=="
    case jp.expr.BinaryExpr.Operator.less => "<"
    case jp.expr.BinaryExpr.Operator.lessEquals => "<="
    case jp.expr.BinaryExpr.Operator.greater => ">"
    case jp.expr.BinaryExpr.Operator.greaterEquals => ">="
    case jp.expr.BinaryExpr.Operator.and => "&&"
    case jp.expr.BinaryExpr.Operator.or => "||"
    case jp.expr.BinaryExpr.Operator.xor => "^"
    case jp.expr.BinaryExpr.Operator.rUnsignedShift => ">>>" 
  }
  implicit def assignmentOperator:
    PartialFunction[jp.expr.AssignExpr.Operator, String] = {
      case jp.expr.AssignExpr.Operator.assign => "="
  }
  implicit def unaryOperator:
    PartialFunction[jp.expr.UnaryExpr.Operator, String] = {
      case jp.expr.UnaryExpr.Operator.inverse => "~"
      case jp.expr.UnaryExpr.Operator.negative => "-"
      case jp.expr.UnaryExpr.Operator.not => "!"
      case jp.expr.UnaryExpr.Operator.posDecrement => "--"
      case jp.expr.UnaryExpr.Operator.posIncrement => "++"
      case jp.expr.UnaryExpr.Operator.positive => "+"
      case jp.expr.UnaryExpr.Operator.preDecrement => "--"
      case jp.expr.UnaryExpr.Operator.preIncrement => "++"
  }
}