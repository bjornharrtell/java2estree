package org.wololo.java2estree.ast

class VariableDeclarator (
  val id: Pattern,
  val init: Expression = null
) extends Node