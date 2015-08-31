package org.wololo.java2estree

import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import org.wololo.estree._
import Converters._
import ExpressionConversions._
import org.eclipse.jdt.core.{ dom => jp }

object StatementConverters {
  def toSwitchCases(x: Buffer[jp.Statement], accu: List[SwitchCase] = List())(implicit td: jp.TypeDeclaration) : List[SwitchCase] = {
    val switchCase = x.head.asInstanceOf[jp.SwitchCase]
    val statements = x.tail.takeWhile { !_.isInstanceOf[jp.SwitchCase] } map { x => toStatement(x) }
    val test = if (switchCase.getExpression == null) null else toExpression(switchCase.getExpression)
    val switchCases = accu :+ new SwitchCase(test, statements)
    if (x.length - 1 - statements.length>0)
      toSwitchCases(x.drop(statements.length + 1), switchCases)
    else 
      switchCases
  }
  
  def toStatement(s: jp.Statement)(implicit td: jp.TypeDeclaration): Statement = s match {
    case x: jp.EmptyStatement =>
      new EmptyStatement()  
    case x: jp.ReturnStatement =>
      new ReturnStatement(toExpression(x.getExpression))
    case x: jp.IfStatement =>
      new IfStatement(toExpression(x.getExpression),
          toStatement(x.getThenStatement),
          toStatement(x.getElseStatement))
    case x: jp.SwitchStatement =>
      val cases = toSwitchCases(x.statements collect { case x: jp.Statement => x })
      new SwitchStatement(toExpression(x.getExpression), cases)
    case x: jp.BreakStatement =>
      new BreakStatement()
    case x: jp.ForStatement =>
      val init = if (x.initializers.size == 1 && x.initializers.get(0).isInstanceOf[jp.VariableDeclarationExpression])
        new VariableDeclaration(x.initializers.get(0).asInstanceOf[jp.VariableDeclarationExpression].fragments map { x => variableDeclarator(x.asInstanceOf[jp.VariableDeclarationFragment]) })
      else
        new SequenceExpression(x.initializers map { x => toExpression(x.asInstanceOf[jp.Expression])})
      val update = new SequenceExpression(x.updaters() map { x => toExpression(x.asInstanceOf[jp.Expression])})
      new ForStatement(init, toExpression(x.getExpression), update, toStatement(x.getBody))
    case x: jp.WhileStatement =>
      new WhileStatement(toExpression(x.getExpression), toStatement(x.getBody))
    case x: jp.ConstructorInvocation =>
      new ExpressionStatement(
          new CallExpression(
              new MemberExpression(
                  new ThisExpression(), new Identifier("init_"), false),
                  x.arguments map { x => toExpression(x.asInstanceOf[jp.Expression]) }
      ))
    case x: jp.Block => blockStatement(x)
    case x: jp.VariableDeclarationStatement =>
      new VariableDeclaration(x.fragments map { x => variableDeclarator(x.asInstanceOf[jp.VariableDeclarationFragment]) })
    case x: jp.ExpressionStatement =>
      new ExpressionStatement(toExpression(x.getExpression))
    
    case x: jp.TryStatement =>
      // TODO: catch switched on exception type
      new TryStatement(blockStatement(x.getBody))
    case x: jp.ThrowStatement => new ThrowStatement(toExpression(x.getExpression))
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