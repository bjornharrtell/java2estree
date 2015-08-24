package org.wololo.estree

trait Node {
  val `type`: String = this.getClass.getSimpleName
}

class Program (
  val sourceType: String,
  val body: List[Statement]
) extends Node

class Function (
  val params: List[Pattern],
  val body: BlockStatement
) extends Node

// Statements

trait Statement extends Node

class EmptyStatement extends Statement

class BlockStatement(val body: List[Statement]) extends Statement

class ExpressionStatement(val expression: Expression) extends Statement

class IfStatement (
  val test: Expression,
  val consequent: Statement,
  val alternate: Statement = null
) extends Statement

class ReturnStatement (
  val argument: Expression
) extends Statement

class ThrowStatement (
  val argument: Expression
) extends Statement

class TryStatement (
  val block: BlockStatement,
  val handler: CatchClause = null,
  val finalizer: BlockStatement = null
) extends Statement

// Declarations

trait Declaration extends Statement

class FunctionDeclaration (
  val id: Identifier,
  params: List[Pattern],
  body: BlockStatement
) extends Function(params, body) with Declaration

class VariableDeclaration (
  val declarations: List[VariableDeclarator]
) extends Declaration {
  val kind: String = "var"
}

class VariableDeclarator (
  val id: Pattern,
  val init: Expression = null
) extends Node

// Expressions

trait Expression extends Node

class ThisExpression extends Expression

class FunctionExpression (
  params: List[Pattern],
  body: BlockStatement
) extends Function(params, body) with Expression

class UnaryExpression (
  val operator: String,
  val prefix: Boolean,
  val argument: Expression
) extends Expression

class BinaryExpression (
  val operator: String,
  val left: Expression,
  val right: Expression
) extends Expression

class AssignmentExpression (
  val operator: String,
  val left: Node,
  val right: Expression
) extends Expression

class CallExpression (
  val callee: Node, // Expression or Super
  val arguments: List[Expression]
) extends Expression

class NewExpression (
  callee: Expression,
  arguments: List[Expression]
)  extends CallExpression(callee, arguments)

class MemberExpression (
  val `object`: Node, // Expression or Super
  val property: Expression,
  val computed: Boolean
) extends Expression with Pattern

class Super extends Node

class ClassExpression(
  body: ClassBody
) extends Class(body) with Expression

// Patterns

trait Pattern extends Node

// Classes

class Class (
  val body: ClassBody
) extends Node

class ClassBody(
  val body: List[MethodDefinition]
) extends Node

class MethodDefinition (
  val key: Expression,
  val value: FunctionExpression,
  val kind: String,
  val computed: Boolean,
  val static: Boolean
) extends Node

class ClassDeclaration (
  val id: Identifier,
  val body: ClassBody
) extends Statement

// Clauses

class CatchClause (
  val param: Pattern,
  val body: BlockStatement
) extends Node

// Miscellaneous

class Identifier (
  val name: String
) extends Node with Expression with Pattern

class Literal (
  val value: Any,
  val raw: String
) extends Node with Expression