package org.wololo.java2estree

import scala.collection.JavaConversions._
import org.wololo.estree._
import Converters._
import ExpressionConversions._
import org.eclipse.jdt.core.{ dom => jp }
import org.eclipse.jdt.core.dom.VariableDeclarationFragment

object StatementConverters {
  def toStatement: PartialFunction[jp.Statement, Statement] = {
    case x: jp.EmptyStatement => new EmptyStatement()  
    case x: jp.ReturnStatement => new ReturnStatement(x.getExpression)
    case x: jp.IfStatement =>
      new IfStatement(x.getExpression, toStatement(x.getThenStatement),
          toStatement(x.getElseStatement))
    case x: jp.ForStatement =>
      // TODO: implement
      new BlockStatement(List())
    case x: jp.WhileStatement =>
      // TODO: implement
      new BlockStatement(List())
    case x: jp.ConstructorInvocation =>
      new ExpressionStatement(
          new CallExpression(
              new MemberExpression(
                  new ThisExpression(), new Identifier("init_"), false),
                  x.arguments map { x => expression(x.asInstanceOf[jp.Expression]) }
      ))
    case x: jp.Block => blockStatement(x)
    case x: jp.VariableDeclarationStatement =>
      new VariableDeclaration(x.fragments map { x => variableDeclarator(x.asInstanceOf[VariableDeclarationFragment]) })
    case x: jp.ExpressionStatement => new ExpressionStatement(x.getExpression)
    // TODO: new TryStatement(blockStatement(x.getTryBlock)) and call catches
    // switched on exception type
    case x: jp.TryStatement => blockStatement(x.getBody)
    case x: jp.ThrowStatement => new ThrowStatement(x.getExpression)
    case null => null
    //case x => {
      //logger.debug(s"Unexpected statement (${if (x==null) x else x.toString()})")
      //new BlockStatement(List())
    //}
  }
  /*
  def statement(es: jp.ExpressionStatement): Statement =
    es.getExpression match {
    case x: jp.expr.VariableDeclarationExpr =>
      new VariableDeclaration(x.getVars map variableDeclarator, "let")
    case x: jp.expr.Expression => new ExpressionStatement(x)
  }*/
}