package org.wololo.java2estree.ast

class MemberExpression (
  val `object`: Expression,
  val property: Expression,
  val computed: Boolean
) extends Expression with Pattern