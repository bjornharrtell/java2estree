package org.wololo.java2estree.ast

class NewExpression (
  callee: Expression,
  arguments: List[Expression] 
)  extends CallExpression(callee, arguments)