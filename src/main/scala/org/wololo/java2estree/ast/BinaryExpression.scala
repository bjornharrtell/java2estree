package org.wololo.java2estree.ast

class BinaryExpression (
  val operator: String,
  val left: Expression,
  val right: Expression
) extends Expression