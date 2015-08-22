package org.wololo.java2estree.ast

class MethodDefinition (
  val key: Expression,
  val value: FunctionExpression,
  val kind: String,
  val computed: Boolean,
  val static: Boolean
) extends Node