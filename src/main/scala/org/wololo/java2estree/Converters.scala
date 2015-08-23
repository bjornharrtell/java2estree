package org.wololo.java2estree

import org.wololo.java2estree.ast._
import scala.collection.JavaConversions._

import com.github.javaparser.{ ast => jp }
import org.wololo.java2estree.ast.Program

object Converters {
  implicit def td2cd(td : jp.body.TypeDeclaration) = new ClassDeclaration(
    new Identifier(td.getName),
    new ClassBody(mapbd(td.getMembers.toList))
  )
  implicit def p2i(p : jp.body.Parameter) = new Identifier(p.getId.getName)
  implicit def vd2vd(vd: jp.body.VariableDeclarator) = new VariableDeclarator(new Identifier(vd.getId.getName), vd.getInit)
  implicit def bs2bs(bs: jp.stmt.BlockStmt) = new BlockStatement(if (bs.getStmts == null) List() else maps(bs.getStmts.toList))
  implicit def o2o: PartialFunction[jp.expr.BinaryExpr.Operator, String] = {
    case jp.expr.BinaryExpr.Operator.plus => "+"
    case jp.expr.BinaryExpr.Operator.minus => "-"
    case jp.expr.BinaryExpr.Operator.times => "*"
    case jp.expr.BinaryExpr.Operator.divide => "/"
    case jp.expr.BinaryExpr.Operator.remainder => "%"
    case jp.expr.BinaryExpr.Operator.less => "<"
    case jp.expr.BinaryExpr.Operator.lessEquals => "<="
    case jp.expr.BinaryExpr.Operator.greater => ">"
    case jp.expr.BinaryExpr.Operator.greaterEquals => ">="
  }
  implicit def e2e: PartialFunction[jp.expr.Expression, Expression] = {
    case nl: jp.expr.NullLiteralExpr =>
      new Literal("null", "null");
    case n: jp.expr.NameExpr =>
      new Identifier(n.getName)
    case ee: jp.expr.EnclosedExpr =>
      ee.getInner
    case il: jp.expr.IntegerLiteralExpr =>
      new Literal(il.getValue, il.getValue)
    case il: jp.expr.LongLiteralExpr =>
      new Literal(il.getValue, il.getValue)
    case il: jp.expr.DoubleLiteralExpr =>
      new Literal(il.getValue, il.getValue)
    case il: jp.expr.CharLiteralExpr =>
      new Literal(il.getValue, "\"" + il.getValue + "\"")
    case il: jp.expr.StringLiteralExpr =>
      new Literal(il.getValue, "\"" + il.getValue + "\"")
    case be: jp.expr.BinaryExpr =>
      new BinaryExpression(be.getOperator, be.getLeft, be.getRight)
    case oc: jp.expr.ObjectCreationExpr =>
      new NewExpression(new Identifier(oc.getType.getName), if (oc.getArgs == null) List() else mape(oc.getArgs.toList))
  }
  implicit def s2s: PartialFunction[jp.stmt.Statement, Statement] = {
    case r: jp.stmt.ReturnStmt => new ReturnStatement(r.getExpr)
    case e: jp.stmt.ExpressionStmt => e2es(e)
  }
  def e2es(es: jp.stmt.ExpressionStmt) : Statement = es.getExpression match {
    case vd: jp.expr.VariableDeclarationExpr =>
      new VariableDeclaration(mapvd(vd.getVars.toList))
    case e: jp.expr.Expression => new ExpressionStatement(e)
  }
  implicit def bd2md: PartialFunction[jp.body.BodyDeclaration, MethodDefinition] = { 
    // TODO: consider Modifiers
    // TODO: consider overloads
    case md: jp.body.ConstructorDeclaration => new MethodDefinition(
      new Identifier("constructor"),
      new FunctionExpression(mapp(md.getParameters.toList), md.getBlock),
      "constructor",
      false,
      false
    )
    case md: jp.body.MethodDeclaration => new MethodDefinition(
      new Identifier(md.getName),
      new FunctionExpression(mapp(md.getParameters.toList), md.getBody),
      "method",
      false,
      false
    )
  }
  
  def asProgram(cu : jp.CompilationUnit) = new Program(maptd(cu.getTypes.toList))
  
  def mapp(l: List[jp.body.Parameter]): List[Identifier] = l map { a => a: Identifier }
  def maps(l: List[jp.stmt.Statement]): List[Statement] = l map { a => a: Statement }
  def mape(l: List[jp.expr.Expression]): List[Expression] = l map { a => a: Expression }
  def maptd(l: List[jp.body.TypeDeclaration]): List[ClassDeclaration] = l map { a => a: ClassDeclaration }
  def mapbd(l: List[jp.body.BodyDeclaration]): List[MethodDefinition] = l map { a => a: MethodDefinition }
  def mapvd(l: List[jp.body.VariableDeclarator]): List[VariableDeclarator] = l map { a => a: VariableDeclarator }
}