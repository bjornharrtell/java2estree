package org.wololo.java2estree.ast

class AssignmentExpression (
  val operator: String,
  val left: Node,
  val right: Expression
) extends Expression