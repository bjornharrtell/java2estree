package org.wololo.java2estree

import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import org.wololo.estree._
import Converters._
import ExpressionConversions._
import org.eclipse.jdt.core.dom

object StatementConverters {
  def toSwitchCases(x: Buffer[dom.Statement], accu: List[SwitchCase] = List())(implicit td: dom.TypeDeclaration) : List[SwitchCase] = {
    val switchCase = x.head.asInstanceOf[dom.SwitchCase]
    val statements = x.tail.takeWhile { !_.isInstanceOf[dom.SwitchCase] } map { x => toStatement(x) }
    val test = if (switchCase.getExpression == null) null else toExpression(switchCase.getExpression)
    val switchCases = accu :+ new SwitchCase(test, statements)
    if (x.length - 1 - statements.length>0)
      toSwitchCases(x.drop(statements.length + 1), switchCases)
    else 
      switchCases
  }
  
  def toVariableDeclarators(fragments: java.util.List[_])(implicit td: dom.TypeDeclaration) =
    fragments collect { case x: dom.VariableDeclarationFragment => toVariableDeclarator(x) }
  
  def toForStatement(x: dom.ForStatement)(implicit td: dom.TypeDeclaration) = {
    val init = if (x.initializers.size == 1 && x.initializers.get(0).isInstanceOf[dom.VariableDeclarationExpression]) {
      val vde = x.initializers.get(0).asInstanceOf[dom.VariableDeclarationExpression]
      new VariableDeclaration(toVariableDeclarators(vde.fragments))
    } 
    else
      new SequenceExpression(toExpressions(x.initializers))
    val update = new SequenceExpression(toExpressions(x.updaters))
    new ForStatement(init, toExpression(x.getExpression), update, toStatement(x.getBody))
  }
  
  def toStatement(s: dom.Statement)(implicit td: dom.TypeDeclaration): Statement = s match {
    case x: dom.EmptyStatement =>
      new EmptyStatement()  
    case x: dom.ReturnStatement =>
      new ReturnStatement(toExpression(x.getExpression))
    case x: dom.IfStatement =>
      new IfStatement(toExpression(x.getExpression),
          toStatement(x.getThenStatement),
          toStatement(x.getElseStatement))
    case x: dom.SwitchStatement =>
      val cases = toSwitchCases(x.statements collect { case x: dom.Statement => x })
      new SwitchStatement(toExpression(x.getExpression), cases)
    case x: dom.ContinueStatement =>
      new ContinueStatement()
    case x: dom.BreakStatement =>
      new BreakStatement()
    case x: dom.ForStatement =>
      toForStatement(x)
    case x: dom.WhileStatement =>
      new WhileStatement(toExpression(x.getExpression), toStatement(x.getBody))
    case x: dom.ConstructorInvocation =>
      val callee = new MemberExpression(new ThisExpression(), new Identifier("init_"), false)
      val call = new CallExpression(callee, toExpressions(x.arguments))
      new ExpressionStatement(call)
    case x: dom.SuperConstructorInvocation =>
      val call = new CallExpression(new Super(), toExpressions(x.arguments))
      new ExpressionStatement(call)
    case x: dom.Block => toBlockStatement(x)
    case x: dom.VariableDeclarationStatement =>
      new VariableDeclaration(toVariableDeclarators(x.fragments))
    case x: dom.ExpressionStatement =>
      new ExpressionStatement(toExpression(x.getExpression))
    case x: dom.TryStatement =>
      // TODO: catch switched on exception type
      new TryStatement(toBlockStatement(x.getBody))
    case x: dom.ThrowStatement => new ThrowStatement(toExpression(x.getExpression))
    case null => null
    //case x => {
      //logger.debug(s"Unexpected statement (${if (x==null) x else x.toString()})")
      //new BlockStatement(List())
    //}
  }
}