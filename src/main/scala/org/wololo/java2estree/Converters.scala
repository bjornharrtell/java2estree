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
  implicit def bs2bs(bs: jp.stmt.BlockStmt) = new BlockStatement(maps(bs.getStmts.toList))
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
    case n: jp.expr.NameExpr =>
      new Identifier(n.getName)
    case il: jp.expr.IntegerLiteralExpr =>
      new Literal(il.getValue, il.getValue)
    case be: jp.expr.BinaryExpr =>
      new BinaryExpression(be.getOperator, be.getLeft, be.getRight)
  }
  implicit def s2s: PartialFunction[jp.stmt.Statement, Statement] = {
    case r: jp.stmt.ReturnStmt => new ReturnStatement(r.getExpr)
    case e: jp.stmt.ExpressionStmt => {
      if (e.getExpression.isInstanceOf[jp.expr.VariableDeclarationExpr]) {
        new VariableDeclaration(mapvd(e.getExpression.asInstanceOf[jp.expr.VariableDeclarationExpr].getVars.toList))
      }
      else new ExpressionStatement(e.getExpression)
    }
  }
  implicit def bd2md: PartialFunction[jp.body.BodyDeclaration, MethodDefinition] = { 
    case md: jp.body.MethodDeclaration => new MethodDefinition(
    new Identifier(md.getName),
    new FunctionExpression(mapp(md.getParameters.toList), md.getBody),
    "method",
    false,
    false
  )}
  
  def asProgram(cu : jp.CompilationUnit) = new Program(maptd(cu.getTypes.toList))
  
  def mapp(l: List[jp.body.Parameter]): List[Identifier] = l map { a => a: Identifier }
  def maps(l: List[jp.stmt.Statement]): List[Statement] = l map { a => a: Statement }
  def maptd(l: List[jp.body.TypeDeclaration]): List[ClassDeclaration] = l map { a => a: ClassDeclaration }
  def mapbd(l: List[jp.body.BodyDeclaration]): List[MethodDefinition] = l map { a => a: MethodDefinition }
  def mapvd(l: List[jp.body.VariableDeclarator]): List[VariableDeclarator] = l map { a => a: VariableDeclarator }
}