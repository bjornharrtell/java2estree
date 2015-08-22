package org.wololo.java2estree.ast

class FunctionExpression (
  params: List[Pattern],
  body: BlockStatement
) extends Function(params, body) with Expression