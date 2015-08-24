package org.wololo.java2estree

import org.wololo.estree._
import Converters._
import ExpressionConversions._
import OperatorConversions._
import CollectionConverters._

import com.github.javaparser.{ ast => jp }

object StatementConverters {
  def statement: PartialFunction[jp.stmt.Statement, Statement] = {
    case x: jp.stmt.EmptyStmt => new EmptyStatement()  
    case x: jp.stmt.ReturnStmt => new ReturnStatement(x.getExpr)
    case x: jp.stmt.IfStmt =>
      new IfStatement(x.getCondition, statement(x.getThenStmt),
          statement(x.getElseStmt))
    case x: jp.stmt.ExplicitConstructorInvocationStmt =>
      new ExpressionStatement(new CallExpression(
          new MemberExpression(new ThisExpression(),
              new Identifier("constructor"), false), expressions(x.getArgs)))
    case x: jp.stmt.BlockStmt => blockStatement(x)
    case x: jp.stmt.ExpressionStmt => statement(x)
    // TODO: new TryStatement(blockStatement(x.getTryBlock)) and call catches
    // switched on exception type
    case x: jp.stmt.TryStmt => blockStatement(x.getTryBlock)
    case x: jp.stmt.ThrowStmt => new ThrowStatement(x.getExpr)
    case null => null
    /*case x => {
      logger.debug(s"Unexpected statement (${if (x==null) x else x.toString()})")
      new BlockStatement(List())
    }*/
  }
  def statement(es: jp.stmt.ExpressionStmt): Statement =
    es.getExpression match {
    case x: jp.expr.VariableDeclarationExpr =>
      new VariableDeclaration(variableDeclarators(x.getVars))
    case x: jp.expr.Expression => new ExpressionStatement(x)
  }
}