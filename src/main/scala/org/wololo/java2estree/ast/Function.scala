package org.wololo.java2estree.ast

class Function (
  val params: List[Pattern],
  val body: BlockStatement
) extends Node