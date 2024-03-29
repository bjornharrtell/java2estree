package org.wololo.estree

trait Node {
  val `type`: String = this.getClass.getSimpleName
}

class Program (
  val sourceType: String,
  val body: Iterable[Node] // [ Statement | ModuleDeclaration ];
) extends Node

class FunctionES5 (
  val params: Iterable[Pattern],
  val body: BlockStatement, // BlockStatement | Expression
  val generator: Boolean = false
) extends Node

class FunctionES6 (
  val params: Iterable[Pattern],
  val body: Node, // BlockStatement | Expression
  val generator: Boolean = false
) extends Node

// Statements

trait Statement extends Node

class EmptyStatement extends Statement

class BlockStatement(val body: Iterable[Statement]) extends Statement

class ExpressionStatement(val expression: Expression) extends Statement

class IfStatement (
  val test: Expression,
  val consequent: Statement,
  val alternate: Statement = null
) extends Statement

class BreakStatement (
  val label: Identifier = null
) extends Statement

class ContinueStatement (
  val label: Identifier = null
) extends Statement

class SwitchStatement (
  val discriminant: Expression,
  val cases: Iterable[SwitchCase]
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

class WhileStatement (
  val test: Expression,
  val body: Statement
) extends Statement

class DoWhileStatement (
  val body: Statement,
  val test: Expression
) extends Statement

class ForStatement (
  val init: Node,
  val test: Expression,
  val update: Expression,
  val body: Statement
) extends Statement

class ForInStatement (
  val left: Node,
  val right: Expression,
  val body: Statement
) extends Statement

class ForOfStatement (
  val left: Node,
  val right: Expression,
  val body: Statement
) extends Statement

// Declarations

trait Declaration extends Statement

class FunctionDeclaration (
  val id: Identifier,
  params: Iterable[Pattern],
  body: BlockStatement,
  generator: Boolean = false
) extends FunctionES5(params, body, generator) with Declaration

class VariableDeclaration (
  val declarations: Iterable[VariableDeclarator],
  val kind: String
) extends Declaration

class VariableDeclarator (
  val id: Pattern,
  val init: Expression = null
) extends Node

// Expressions

trait Expression extends Node

class ThisExpression extends Expression

class ArrayExpression(
  val elements: Iterable[Expression]
) extends Expression

class ObjectExpression(
  val properties: Iterable[Property]
) extends Expression

class Property(
  val key: Expression,
  val value: Expression,
  val kind: String = "init",
  val computed: Boolean = false,
  val method: Boolean = false,
  val shorthand: Boolean = false
) extends Node {
  def this(key: String, value: Expression) = {
    this(new Identifier(key), value)
  }
}

class FunctionExpression (
  params: Iterable[Pattern],
  body: BlockStatement,
  generator: Boolean = false
) extends FunctionES5(params, body, generator) with Expression

class SequenceExpression (
  val expressions: Iterable[Expression]
) extends Expression

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

class LogicalExpression (
  val operator: String,
  val left: Expression,
  val right: Expression
) extends Expression

class ConditionalExpression (
  val test: Expression,
  val alternate: Expression,
  val consequent: Expression
) extends Expression

class CallExpression (
  val callee: Node, // Expression or Super
  val arguments: Iterable[Node] = List()// Expression or SpreadElement
) extends Expression

class NewExpression (
  callee: Expression,
  arguments: Iterable[Expression]
)  extends CallExpression(callee, arguments)

class MemberExpression (
  val `object`: Node, // Expression or Super
  val property: Expression,
  val computed: Boolean = false
) extends Expression with Pattern {
  def this(`object`: String, property: String) = {
    this(new Identifier(`object`), new Identifier(property)) 
  }
  def this(`object`: Node, property: String) = {
    this(`object`, new Identifier(property)) 
  }
  def this(`object`: String, property: Expression) = {
    this(new Identifier(`object`), property) 
  }
}

class ArrowFunctionExpression(
  params: Iterable[Pattern],
  body: Node, // BlockStatement | Expression
  val expression: Boolean,
  generator: Boolean = false
) extends FunctionES6(params, body, generator) with Expression

class Super extends Node

class ClassExpression(
  body: ClassBody,
  superClass: Expression
) extends Class(body, superClass) with Expression

class SpreadElement (
  val argument: Expression
) extends Node



// Patterns

trait Pattern extends Node

class ArrayPattern (
  val elements: Iterable[Pattern]
) extends Pattern

class RestElement (
  val argument: Pattern
) extends Pattern

// Classes

class Class (
  val body: ClassBody,
  val superClass: Expression
) extends Node

class ClassBody(
  val body: Iterable[MethodDefinition]
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
  body: ClassBody,
  superClass: Expression
) extends Class(body, superClass) with Declaration

// Clauses

class SwitchCase (
  val test: Expression,
  val consequent: Iterable[Statement]
) extends Node

class CatchClause (
  val param: Pattern,
  val body: BlockStatement
) extends Node

// Modules

trait ModuleDeclaration extends Node

class ModuleSpecifier (
  val local: Identifier
) extends Node

class ImportDeclaration (
  val specifiers: Iterable[Node], // [ ImportSpecifier | ImportDefaultSpecifier | ImportNamespaceSpecifier ]
  val source: Literal
) extends ModuleDeclaration

class ImportSpecifier (
  local: Identifier,
  val imported: Identifier
) extends ModuleSpecifier(local)

class ImportDefaultSpecifier (
  local: Identifier
) extends ModuleSpecifier(local)

class ImportNamespaceSpecifier (
  local: Identifier
) extends ModuleSpecifier(local)

class ExportNamedDeclaration (
  val declaration: Declaration = null,
  val specifiers: Iterable[ ExportSpecifier ],
  val source: Literal = null
) extends ModuleDeclaration

class ExportSpecifier (
  local: Identifier,
  val exported: Identifier
) extends ModuleSpecifier(local)

class ExportDefaultDeclaration (
  val declaration: Node //Declaration | Expression;
) extends ModuleDeclaration

class ExportAllDeclaration (
  source: Literal
) extends ModuleDeclaration

// Miscellaneous

class Identifier (
  val name: String
) extends Node with Expression with Pattern

class Literal (
  val value: Any,
  val raw: String
) extends Node with Expression