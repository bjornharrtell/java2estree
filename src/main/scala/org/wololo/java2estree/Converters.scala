package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import com.github.javaparser.{ ast => jp }
import java.lang.reflect.Modifier

import com.typesafe.scalalogging.LazyLogging

object Converters extends LazyLogging {
  def classDeclaration(td : jp.body.TypeDeclaration) =
    new ClassDeclaration(
      new Identifier(td.getName),
      new ClassBody(bodyDeclarations(td.getMembers))
  )
  def identifier(p : jp.body.Parameter) = new Identifier(p.getId.getName)
  def variableDeclarator(vd: jp.body.VariableDeclarator) =
    new VariableDeclarator(new Identifier(vd.getId.getName), vd.getInit)
  def blockStatement(bs: jp.stmt.BlockStmt) =
    new BlockStatement(statements(bs.getStmts))
  def binaryOperator: PartialFunction[jp.expr.BinaryExpr.Operator, String] = {
    case jp.expr.BinaryExpr.Operator.plus => "+"
    case jp.expr.BinaryExpr.Operator.minus => "-"
    case jp.expr.BinaryExpr.Operator.times => "*"
    case jp.expr.BinaryExpr.Operator.divide => "/"
    case jp.expr.BinaryExpr.Operator.remainder => "%"
    case jp.expr.BinaryExpr.Operator.equals => "==="
    case jp.expr.BinaryExpr.Operator.notEquals => "!=="
    case jp.expr.BinaryExpr.Operator.less => "<"
    case jp.expr.BinaryExpr.Operator.lessEquals => "<="
    case jp.expr.BinaryExpr.Operator.greater => ">"
    case jp.expr.BinaryExpr.Operator.greaterEquals => ">="
    case jp.expr.BinaryExpr.Operator.and => "&&"
    case jp.expr.BinaryExpr.Operator.or => "||"
  }
  def assignmentOperator:
    PartialFunction[jp.expr.AssignExpr.Operator, String] = {
      case jp.expr.AssignExpr.Operator.assign => "="
  }
  implicit def expression: PartialFunction[jp.expr.Expression, Expression] = {
    case nl: jp.expr.NullLiteralExpr => new Literal("null", "null")
    case n: jp.expr.NameExpr => new Identifier(n.getName)
    case x: jp.expr.AssignExpr =>
      new AssignmentExpression(
          assignmentOperator(x.getOperator), x.getTarget, x.getValue)
    case ee: jp.expr.EnclosedExpr => ee.getInner
    case x: jp.expr.BooleanLiteralExpr =>
      new Literal(x.getValue, x.getValue.toString)
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
      new BinaryExpression(
          binaryOperator(be.getOperator), be.getLeft, be.getRight)
    case oc: jp.expr.ObjectCreationExpr =>
      new NewExpression(
          new Identifier(oc.getType.getName), expressions(oc.getArgs))
    case x: jp.expr.FieldAccessExpr =>
      new MemberExpression(
          new ThisExpression(), new Identifier(x.getField), false)
    case x: jp.expr.MethodCallExpr =>
      new CallExpression(new MemberExpression(new ThisExpression(),
          new Identifier(x.getName), false), expressions(x.getArgs))
    case _ => {
      logger.debug("Unexpected statement")
      new Literal("null", "null")
    }
  }
  def statement: PartialFunction[jp.stmt.Statement, Statement] = {
    case x: jp.stmt.ReturnStmt => new ReturnStatement(x.getExpr)
    case x: jp.stmt.IfStmt =>
      new IfStatement(x.getCondition, statement(x.getThenStmt),
          statement(x.getElseStmt))
    case x: jp.stmt.ExplicitConstructorInvocationStmt =>
      // TODO: call with apply
      new ExpressionStatement(new CallExpression(
          new MemberExpression(new ThisExpression(),
              new Identifier("constructor"), false), expressions(x.getArgs)))
    case x: jp.stmt.BlockStmt => blockStatement(x)
    case x: jp.stmt.ExpressionStmt => statement(x)
    case _ => {
      logger.debug("Unexpected statement")
      new BlockStatement(List())}
  }
  def statement(es: jp.stmt.ExpressionStmt): Statement =
    es.getExpression match {
    case x: jp.expr.VariableDeclarationExpr =>
      new VariableDeclaration(variableDeclarators(x.getVars))
    case x: jp.expr.Expression => new ExpressionStatement(x)
  }
  def methodDefinitions:
    PartialFunction[jp.body.BodyDeclaration, List[MethodDefinition]] = {
    case x: jp.body.FieldDeclaration => x.getVariables.toList map {
      // TODO: need setter if non-final
      x => new MethodDefinition(
        new Identifier(x.getId.getName),
        new FunctionExpression(List(), new BlockStatement(
            if (x.getInit == null) List() else
              List(new ReturnStatement(x.getInit)))),
        "get",
        false,
        false
      )
    }
    // TODO: need to create logic for overloaded constructors
    case md: jp.body.ConstructorDeclaration => List(new MethodDefinition(
      new Identifier("constructor"),
      new FunctionExpression(parameters(md.getParameters),
          blockStatement(md.getBlock)),
      "constructor",
      false,
      false
    ))
    case md: jp.body.MethodDeclaration => List(new MethodDefinition(
      new Identifier(md.getName),
      new FunctionExpression(parameters(md.getParameters),
          blockStatement(md.getBody)),
      "method",
      false,
      Modifier.isStatic(md.getModifiers)
    ))
    case x: jp.body.ClassOrInterfaceDeclaration => {
      logger.debug("Inner class not supported yet")
      List()
    }
  }

  def program(cu : jp.CompilationUnit) =
    new Program("module", typeDeclarations(cu.getTypes))
  def parameters(l: java.util.List[jp.body.Parameter]): List[Identifier] =
    l.toList map { a => identifier(a) }
  def statements(l: java.util.List[jp.stmt.Statement]): List[Statement] =
    if (l == null) List() else l.toList map { a => statement(a) }
  def expressions(l: java.util.List[jp.expr.Expression]): List[Expression] =
    if (l == null) List() else l.toList map { a => a: Expression }
  def typeDeclarations(l: java.util.List[jp.body.TypeDeclaration]):
    List[ClassDeclaration] = l.toList map { a => classDeclaration(a) }
  def bodyDeclarations(l: java.util.List[jp.body.BodyDeclaration]):
    List[MethodDefinition] = l.toList flatMap { a => methodDefinitions(a) }
  def variableDeclarators(l: java.util.List[jp.body.VariableDeclarator]):
    List[VariableDeclarator] = l.toList map { a => variableDeclarator(a) }
}