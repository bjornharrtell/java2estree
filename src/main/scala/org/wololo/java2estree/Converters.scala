package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import com.github.javaparser.{ ast => jp }
import java.lang.reflect.Modifier
import OperatorConversions._
import ExpressionConversions._
import CollectionConverters._
import StatementConverters._

import com.typesafe.scalalogging.LazyLogging

object Converters extends LazyLogging {
  def classDeclaration(td : jp.body.TypeDeclaration) =
    new ClassDeclaration(new Identifier(td.getName),
      new ClassBody(bodyDeclarations(td.getMembers)))
  
  def classExpression(cd : jp.body.ClassOrInterfaceDeclaration) =
    new ClassExpression(new ClassBody(bodyDeclarations(cd.getMembers)))
  
  def identifier(p : jp.body.Parameter) = new Identifier(p.getId.getName)
  
  def variableDeclarator(vd: jp.body.VariableDeclarator) =
    new VariableDeclarator(new Identifier(vd.getId.getName), vd.getInit)
  
  def blockStatement(bs: jp.stmt.BlockStmt) =
    new BlockStatement(statements(bs.getStmts))
  
  def program(cu : jp.CompilationUnit) : Program =
    new Program("module", typeDeclarations(cu.getTypes))
  
}