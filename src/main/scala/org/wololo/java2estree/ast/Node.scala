package org.wololo.java2estree.ast

trait Node {
  val `type`: String = this.getClass.getSimpleName
}