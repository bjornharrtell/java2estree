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

  def fromCatchClauses(clauses: Iterable[dom.CatchClause], name: Identifier)(implicit td: dom.TypeDeclaration): Statement = {
    if (clauses.size > 0) {
      val test = toInstanceOf(name, clauses.head.getException.getType.resolveBinding().getName)
      val consequent = fromBlock(clauses.head.getBody)
      new IfStatement(test, consequent, fromCatchClauses(clauses.tail, name))
    } else {
      new ThrowStatement(name)
    }
  }
  
  def fromFragments(fragments: java.util.List[_])(implicit td: dom.TypeDeclaration) = 
    fragments collect { case x: dom.VariableDeclarationFragment => fromVariableDeclarationFragment(x) }
  
  def fromForStatement(x: dom.ForStatement)(implicit td: dom.TypeDeclaration) = {
    val init = if (x.initializers.size == 0) null 
    else if (x.initializers.size == 1 && x.initializers.get(0).isInstanceOf[dom.VariableDeclarationExpression]) {
      val vde = x.initializers.get(0).asInstanceOf[dom.VariableDeclarationExpression]
      new VariableDeclaration(fromFragments(vde.fragments), "var")
    } 
    else
      new SequenceExpression(toExpressions(x.initializers))
    val update: Expression = if (x.updaters.size() == 0) null
    else if (x.updaters.size() == 1) toExpressions(x.updaters).head
    else new SequenceExpression(toExpressions(x.updaters))
    new ForStatement(init, toExpression(x.getExpression), update, fromStatement(x.getBody))
  }
  
  def fromStatement(s: dom.Statement)(implicit td: dom.TypeDeclaration): Statement = s match {
    case x: dom.EmptyStatement =>
      new EmptyStatement()  
    case x: dom.ReturnStatement =>
      new ReturnStatement(toExpression(x.getExpression))
    case x: dom.IfStatement =>
      val consequent = fromStatement(x.getThenStatement)
      val alternate = fromStatement(x.getElseStatement)
      new IfStatement(toExpression(x.getExpression), consequent, alternate)
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
      val overloadsCall = new MemberExpression(new Identifier("overloads"), new Identifier("call"), false)
      val call = new CallExpression(overloadsCall, new ThisExpression +: toExpressions(x.arguments))
      new ExpressionStatement(call)
    case x: dom.SuperConstructorInvocation =>
      val call = new CallExpression(new Super, toExpressions(x.arguments))
      new ExpressionStatement(call)
    case x: dom.Block => fromBlock(x)
    case x: dom.VariableDeclarationStatement =>
      new VariableDeclaration(fromFragments(x.fragments), "var")
    case x: dom.ExpressionStatement =>
      new ExpressionStatement(toExpression(x.getExpression))
    case x: dom.TryStatement =>
      val block = fromBlock(x.getBody)
      val finalizer = fromBlock(x.getFinally)
      if (x.catchClauses.size()>0) {
        val name = new Identifier(x.catchClauses().get(0).asInstanceOf[dom.CatchClause].getException.getName.getIdentifier)
        val cases = new BlockStatement(List(fromCatchClauses(x.catchClauses collect { case x: dom.CatchClause => x}, name)))
        val handler = new CatchClause(name, cases)
        new TryStatement(block, handler, finalizer)
      } else {
        new TryStatement(block, null, finalizer)
      }
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