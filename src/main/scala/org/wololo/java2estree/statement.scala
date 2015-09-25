package org.wololo.java2estree

import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer
import org.wololo.estree._
import compilationunit._
import expression._
import org.eclipse.jdt.core.dom

object statement {
  def fromVariableDeclarationFragment(vd: dom.VariableDeclarationFragment)(implicit td: dom.TypeDeclaration) = 
    new VariableDeclarator(new Identifier(vd.getName.getIdentifier), toExpression(vd.getInitializer))
  
  def fromSwitchCases(x: Buffer[dom.Statement], accu: List[SwitchCase] = List())(implicit td: dom.TypeDeclaration): List[SwitchCase] = {
    val switchCase = x.head.asInstanceOf[dom.SwitchCase]
    val statements = x.tail.takeWhile { !_.isInstanceOf[dom.SwitchCase] } map { x => fromStatement(x) }
    val test = if (switchCase.getExpression == null) null else toExpression(switchCase.getExpression)
    val switchCases = accu :+ new SwitchCase(test, statements)
    if (x.length - 1 - statements.length>0)
      fromSwitchCases(x.drop(statements.length + 1), switchCases)
    else 
      switchCases
  }

  def fromCatchClauses(clauses: Iterable[dom.CatchClause])(implicit td: dom.TypeDeclaration): IfStatement = {
    if (clauses.size > 0) {
      val test = toInstanceOf(new Identifier("e"), clauses.head.getException.getType.resolveBinding().getName)
      val consequent = fromBlock(clauses.head.getBody)
      new IfStatement(test, consequent, fromCatchClauses(clauses.tail))
    } else null
  }
  
  def fromFragments(fragments: java.util.List[_])(implicit td: dom.TypeDeclaration) = 
    fragments collect { case x: dom.VariableDeclarationFragment => fromVariableDeclarationFragment(x) }
  
  def fromForStatement(x: dom.ForStatement)(implicit td: dom.TypeDeclaration) = {
    val init = if (x.initializers.size == 1 && x.initializers.get(0).isInstanceOf[dom.VariableDeclarationExpression]) {
      val vde = x.initializers.get(0).asInstanceOf[dom.VariableDeclarationExpression]
      new VariableDeclaration(fromFragments(vde.fragments))
    } 
    else
      new SequenceExpression(toExpressions(x.initializers))
    val update = new SequenceExpression(toExpressions(x.updaters))
    new ForStatement(init, toExpression(x.getExpression), update, fromStatement(x.getBody))
  }
  
  def fromStatement(s: dom.Statement)(implicit td: dom.TypeDeclaration): Statement = s match {
    case x: dom.EmptyStatement =>
      new EmptyStatement()  
    case x: dom.ReturnStatement =>
      new ReturnStatement(toExpression(x.getExpression))
    case x: dom.IfStatement =>
      new IfStatement(toExpression(x.getExpression),          fromStatement(x.getThenStatement),          fromStatement(x.getElseStatement))
    case x: dom.SwitchStatement =>
      val cases = fromSwitchCases(x.statements collect { case x: dom.Statement => x })
      new SwitchStatement(toExpression(x.getExpression), cases)
    case x: dom.ContinueStatement =>
      new ContinueStatement()
    case x: dom.BreakStatement =>
      new BreakStatement()
    case x: dom.ForStatement =>
      fromForStatement(x)
    case x: dom.EnhancedForStatement =>
      val left = toExpression(x.getParameter.getInitializer)
      val right = toExpression(x.getExpression) 
      val body = fromStatement(x.getBody)
      new ForInStatement(left, right, body)
    case x: dom.WhileStatement =>
      new WhileStatement(toExpression(x.getExpression), fromStatement(x.getBody))
    case x: dom.DoStatement =>
      new DoWhileStatement(fromStatement(x.getBody), toExpression(x.getExpression))
    case x: dom.ConstructorInvocation =>
      val callee = new MemberExpression(new ThisExpression(), new Identifier("init_"), false)
      val call = new CallExpression(callee, toExpressions(x.arguments))
      new ExpressionStatement(call)
    case x: dom.SuperConstructorInvocation =>
      //val call = new CallExpression(new Super(), toExpressions(x.arguments))
      //new ExpressionStatement(call)
      val callee = new MemberExpression(new Super(), new Identifier("init_"), false)
      val call = new CallExpression(callee, toExpressions(x.arguments))
      new ExpressionStatement(call)
    case x: dom.Block => fromBlock(x)
    case x: dom.VariableDeclarationStatement =>
      new VariableDeclaration(fromFragments(x.fragments))
    case x: dom.ExpressionStatement =>
      new ExpressionStatement(toExpression(x.getExpression))
    case x: dom.TryStatement =>
      val block = fromBlock(x.getBody)
      val cases = new BlockStatement(List(fromCatchClauses(x.catchClauses collect { case x: dom.CatchClause => x})))
      val handler = new CatchClause(new Identifier("e"), cases)
      val finalizer = fromBlock(x.getFinally)
      new TryStatement(block, handler, finalizer)
    case x: dom.ThrowStatement => new ThrowStatement(toExpression(x.getExpression))
    case x: dom.SynchronizedStatement =>
      fromBlock(x.getBody)
    case x: dom.LabeledStatement =>
      new EmptyStatement()
    case null => null
    //case x => {
      //logger.debug(s"Unexpected statement (${if (x==null) x else x.toString()})")
      //new BlockStatement(List())
    //}
  }
}