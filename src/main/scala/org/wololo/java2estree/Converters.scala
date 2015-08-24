package org.wololo.java2estree

import org.wololo.estree._
import scala.collection.JavaConversions._
import com.github.javaparser.{ ast => jp }
import java.lang.reflect.Modifier
import OperatorConversions._
import ExpressionConversions._
import CollectionConverters._
import StatementConverters._

import com.typesafe.scalalogging.LazyLogging

object Converters extends LazyLogging {
  def classDeclaration(td : jp.body.TypeDeclaration) =
    new ClassDeclaration(new Identifier(td.getName),
      new ClassBody(bodyDeclarations(td.getMembers))
  )
  def classExpression(cd : jp.body.ClassOrInterfaceDeclaration) =
    new ClassExpression(new ClassBody(bodyDeclarations(cd.getMembers))
  )
  def identifier(p : jp.body.Parameter) = new Identifier(p.getId.getName)
  def variableDeclarator(vd: jp.body.VariableDeclarator) =
    new VariableDeclarator(new Identifier(vd.getId.getName), vd.getInit)
  def blockStatement(bs: jp.stmt.BlockStmt) =
    new BlockStatement(statements(bs.getStmts))
  
  def methodDefinitions:
    PartialFunction[jp.body.BodyDeclaration, List[MethodDefinition]] = {
    case x: jp.body.FieldDeclaration => 
      x.getVariables.toList map {
      // TODO: need setter and private/hidden member if non-final
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
    case x: jp.body.ClassOrInterfaceDeclaration => List(new MethodDefinition(
      new Identifier(x.getName),
      new FunctionExpression(parameters(List()),
          new BlockStatement(List(new ReturnStatement(classExpression(x))))),
          //blockStatement(classDeclaration(x))),
      "get",
      false,
      Modifier.isStatic(x.getModifiers)
    ))
  }

  def program(cu : jp.CompilationUnit) : Program = {
    new Program("module", typeDeclarations(cu.getTypes))
  }
}