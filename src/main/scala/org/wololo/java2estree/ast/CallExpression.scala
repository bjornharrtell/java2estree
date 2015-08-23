package org.wololo.java2estree.ast

class CallExpression (
  val callee: Expression,
  val arguments: List[Expression] 
) extends Expression