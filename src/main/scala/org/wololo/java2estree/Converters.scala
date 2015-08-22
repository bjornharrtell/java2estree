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
  implicit def bs2bs(bs: jp.stmt.BlockStmt) = new BlockStatement(maps(bs.getStmts.toList))
  implicit def o2o: PartialFunction[jp.expr.BinaryExpr.Operator, String] = {
    case jp.expr.BinaryExpr.Operator.plus => "+"
  }
  implicit def e2e: PartialFunction[jp.expr.Expression, Expression] = {
    case be: com.github.javaparser.ast.expr.BinaryExpr =>
      new BinaryExpression(be.getOperator, be.getLeft, be.getRight)
    case n: jp.expr.NameExpr =>
      new Identifier(n.getName)
  }
  implicit def s2s: PartialFunction[jp.stmt.Statement, Statement] = {
    case r: jp.stmt.ReturnStmt => new ReturnStatement(r.getExpr)
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
}