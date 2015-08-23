package org.wololo.java2estree.ast

class VariableDeclaration (
  val declarations: List[VariableDeclarator]
) extends Declaration {
  val kind: String = "var"
}