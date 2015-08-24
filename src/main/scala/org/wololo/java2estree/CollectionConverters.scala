package org.wololo.java2estree

import scala.collection.JavaConversions._

import org.wololo.estree._
import Converters._
import OperatorConversions._
import ExpressionConversions._
import StatementConverters._

import com.github.javaparser.{ ast => jp }

object CollectionConverters {
  def parameters(l: java.util.List[jp.body.Parameter]): List[Identifier] =
    l.toList map { a => identifier(a) }
  def statements(l: java.util.List[jp.stmt.Statement]): List[Statement] =
    if (l == null) List() else l.toList map { a => statement(a) }
  def expressions(l: java.util.List[jp.expr.Expression]): List[Expression] =
    if (l == null) List() else l.toList map { a => expression(a) }
  def typeDeclarations(l: java.util.List[jp.body.TypeDeclaration]):
    List[ClassDeclaration] = l.toList map { a => classDeclaration(a) }
  def bodyDeclarations(l: java.util.List[jp.body.BodyDeclaration]):
    List[MethodDefinition] = l.toList flatMap { a => methodDefinitions(a) }
  def variableDeclarators(l: java.util.List[jp.body.VariableDeclarator]):
    List[VariableDeclarator] = l.toList map { a => variableDeclarator(a) }
}