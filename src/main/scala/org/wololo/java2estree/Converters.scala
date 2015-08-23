package org.wololo.java2estree

import org.wololo.java2estree.ast._
import scala.collection.JavaConversions._
import com.github.javaparser.{ ast => jp }
import org.wololo.java2estree.ast.Program
import java.lang.reflect.Modifier

object Converters {
  implicit def td2cd(td : jp.body.TypeDeclaration) = new ClassDeclaration(
    new Identifier(td.getName),
    new ClassBody(bodyDeclarations(td.getMembers))
  )
  implicit def p2i(p : jp.body.Parameter) = new Identifier(p.getId.getName)
  implicit def vd2vd(vd: jp.body.VariableDeclarator) = new VariableDeclarator(new Identifier(vd.getId.getName), vd.getInit)
  implicit def bs2bs(bs: jp.stmt.BlockStmt) = new BlockStatement(if (bs.getStmts == null) List() else statements(bs.getStmts))
  implicit def o2s: PartialFunction[jp.expr.BinaryExpr.Operator, String] = {
    case jp.expr.BinaryExpr.Operator.plus => "+"
    case jp.expr.BinaryExpr.Operator.minus => "-"
    case jp.expr.BinaryExpr.Operator.times => "*"
    case jp.expr.BinaryExpr.Operator.divide => "/"
    case jp.expr.BinaryExpr.Operator.remainder => "%"
    case jp.expr.BinaryExpr.Operator.equals => "==="
    case jp.expr.BinaryExpr.Operator.less => "<"
    case jp.expr.BinaryExpr.Operator.lessEquals => "<="
    case jp.expr.BinaryExpr.Operator.greater => ">"
    case jp.expr.BinaryExpr.Operator.greaterEquals => ">="
  }
  implicit def ao2s: PartialFunction[jp.expr.AssignExpr.Operator, String] = {
    case jp.expr.AssignExpr.Operator.assign => "="
  }
  implicit def e2e: PartialFunction[jp.expr.Expression, Expression] = {
    case nl: jp.expr.NullLiteralExpr => new Literal("null", "null");
    case n: jp.expr.NameExpr => new Identifier(n.getName)
    case x: jp.expr.AssignExpr => new AssignmentExpression(x.getOperator, x.getValue, x.getTarget)
    case ee: jp.expr.EnclosedExpr => ee.getInner
    case il: jp.expr.IntegerLiteralExpr => new Literal(il.getValue, il.getValue)
    case il: jp.expr.LongLiteralExpr => new Literal(il.getValue, il.getValue)
    case il: jp.expr.DoubleLiteralExpr => new Literal(il.getValue, il.getValue)
    case il: jp.expr.CharLiteralExpr => new Literal(il.getValue, "\"" + il.getValue + "\"")
    case il: jp.expr.StringLiteralExpr => new Literal(il.getValue, "\"" + il.getValue + "\"")
    case be: jp.expr.BinaryExpr => new BinaryExpression(be.getOperator, be.getLeft, be.getRight)
    case oc: jp.expr.ObjectCreationExpr =>
      new NewExpression(new Identifier(oc.getType.getName), if (oc.getArgs == null) List() else expressions(oc.getArgs))
  }
  def statement: PartialFunction[jp.stmt.Statement, Statement] = {
    case x: jp.stmt.ReturnStmt => new ReturnStatement(x.getExpr)
    case x: jp.stmt.IfStmt => new IfStatement(x.getCondition, statement(x.getThenStmt) , statement(x.getElseStmt))
    case x: jp.stmt.BlockStmt => x //{println(x.toString); new BlockStatement(List())}
    case x: jp.stmt.ExpressionStmt => e2es(x)
  }
  def e2es(es: jp.stmt.ExpressionStmt) : Statement = es.getExpression match {
    case x: jp.expr.VariableDeclarationExpr =>
      new VariableDeclaration(variableDeclarators(x.getVars))
    case x: jp.expr.Expression => new ExpressionStatement(x)
  }
  implicit def bd2md: PartialFunction[jp.body.BodyDeclaration, MethodDefinition] = { 
    // TODO: consider Modifiers
    // TODO: consider overloads
    case md: jp.body.ConstructorDeclaration => new MethodDefinition(
      new Identifier("constructor"),
      new FunctionExpression(parameters(md.getParameters), md.getBlock),
      "constructor",
      false,
      false
    )
    case md: jp.body.MethodDeclaration => new MethodDefinition(
      new Identifier(md.getName),
      new FunctionExpression(parameters(md.getParameters), md.getBody),
      "method",
      false,
      Modifier.isStatic(md.getModifiers)
    )
  }
  
  def asProgram(cu : jp.CompilationUnit) = new Program(typeDeclarations(cu.getTypes))
  
  def parameters(l: java.util.List[jp.body.Parameter]): List[Identifier] = l.toList map { a => a: Identifier }
  def statements(l: java.util.List[jp.stmt.Statement]): List[Statement] = l.toList map { a => statement(a) }
  def expressions(l: java.util.List[jp.expr.Expression]): List[Expression] = l.toList map { a => a: Expression }
  def typeDeclarations(l: java.util.List[jp.body.TypeDeclaration]): List[ClassDeclaration] = l.toList map { a => a: ClassDeclaration }
  def bodyDeclarations(l: java.util.List[jp.body.BodyDeclaration]): List[MethodDefinition] = l.toList map { a => a: MethodDefinition }
  def variableDeclarators(l: java.util.List[jp.body.VariableDeclarator]): List[VariableDeclarator] = l.toList map { a => a: VariableDeclarator }
}