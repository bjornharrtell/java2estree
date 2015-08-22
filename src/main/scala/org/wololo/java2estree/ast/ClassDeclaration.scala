package org.wololo.java2estree.ast

class ClassDeclaration (
  val id: Identifier,
  val body: ClassBody
) extends Statement