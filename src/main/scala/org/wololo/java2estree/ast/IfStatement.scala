package org.wololo.java2estree.ast

class IfStatement (
  val test: Expression,
  val consequent: Statement,
  val alternate: Statement = null
) extends Statement