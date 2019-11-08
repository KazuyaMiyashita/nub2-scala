package nub2

import nub2.Ast.BinaryOperator._

object Ast {

  trait ExpressionVisitor[E] {
    def visitBinaryExpression(node: BinaryExpression): E
    def visitIntLiteral(node: IntLiteral): E
    def visitBooleanLiteral(node: BooleanLiteral): E
    def visitStringLiteral(node: StringLiteral): E
    def visitLetExpression(node: LetExpression): E
    def visitId(node: Id): E
    def visitBlock(node: Block): E
    def visitIfExpression(node: IfExpression): E
    def visitWhileExpression(node: WhileExpression): E
    def visitAssignmentExpression(node: AssignmentExpression): E
    def visitPrintlnExpression(node: PrintlnExpression): E
    def visitDefFunction(node: DefFunction): E
    def visitFunctionCall(node: FunctionCall): E
  }

  trait Node

  trait Expression extends Node {
    def accept[E](visitor: ExpressionVisitor[E]): E
  }

  case class FunctionCall(name: String, params: List[Ast.Expression]) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitFunctionCall(this)
    }
  }

  case class DefFunction(name: String, args: List[String], body: Block) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitDefFunction(this)
    }
  }

  case class LetExpression(variableName: String, init: Ast.Expression, body: Ast.Block) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitLetExpression(this)
    }
  }

  case class AssignmentExpression(variableName: String, expression: Ast.Expression) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitAssignmentExpression(this)
    }
  }

  case class IfExpression(condition: Ast.Expression, thenClause: Ast.Block, elseClause: Ast.Block) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitIfExpression(this)
    }
  }

  case class WhileExpression(condition: Ast.Expression, body: List[Ast.Expression]) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitWhileExpression(this)
    }
  }
  object WhileExpression {
    def apply(condition: Ast.Expression, body: Ast.Expression*): WhileExpression = {
      new WhileExpression(condition, body.toList)
    }
  }

  case class PrintlnExpression(target: Ast.Expression) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitPrintlnExpression(this)
    }
  }

  case class Block(expressions: List[Expression]) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitBlock(this)
    }
  }
  object Block {
    def apply(expressions: Expression*): Block = {
      new Block(expressions.toList)
    }
  }

  sealed abstract class BinaryOperator(val op: String)
  object BinaryOperator {
    case object ADD extends BinaryOperator("+")
    case object SUBTRACT extends BinaryOperator("+")
    case object MULTIPLY extends BinaryOperator("*")
    case object DIVIDE extends BinaryOperator("/")
    case object LESS_THAN extends BinaryOperator("<")
    case object LESS_THAN_OR_EQUAL extends BinaryOperator("<=")
    case object GREATER_THAN extends BinaryOperator(">")
    case object GREATER_THAN_OR_EQUAL extends BinaryOperator(">=")
    case object EQUAL extends BinaryOperator("==")
    case object NOT_EQUAL extends BinaryOperator("!=")
    case object LOGICAL_AND extends BinaryOperator("&&")
    case object LOGCIAL_OR extends BinaryOperator("||")
  }

  case class BinaryExpression(operator: BinaryOperator, lhs: Expression, rhs: Expression) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitBinaryExpression(this)
    }
  }

  case class IntLiteral(value: Int) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitIntLiteral(this)
    }
  }

  case class StringLiteral(value: String) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitStringLiteral(this)
    }
  }

  case class BooleanLiteral(value: Boolean) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitBooleanLiteral(this)
    }
  }

  case class Id(name: String) extends Expression {
    override def accept[E](visitor: ExpressionVisitor[E]): E = {
      visitor.visitId(this)
    }
  }

  object Factory {

    // Literals
    def tString(value: String): StringLiteral = new StringLiteral(value)
    def tInt(value: Int): IntLiteral = new IntLiteral(value)
    def tBoolean(value: Boolean): BooleanLiteral = new BooleanLiteral(value)
    def tId(name: String): Id = new Id(name)

    // Println expression
    def tPrintln(target: Expression): PrintlnExpression = new PrintlnExpression(target)

    // Binary expressions
    def tAdd(lhs: Expression, rhs: Expression): BinaryExpression = new BinaryExpression(ADD ,lhs, rhs)
    def tSubtract(lhs: Expression, rhs: Expression): BinaryExpression = new BinaryExpression(SUBTRACT ,lhs, rhs)
    def tMultiply(lhs: Expression, rhs: Expression): BinaryExpression = new BinaryExpression(MULTIPLY ,lhs, rhs)
    def tDivide(lhs: Expression, rhs: Expression): BinaryExpression = new BinaryExpression(DIVIDE ,lhs, rhs)
    def tAnd(lhs: Expression, rhs: Expression): BinaryExpression = new BinaryExpression(LOGICAL_AND, lhs, rhs)
    def tOr(lhs: Expression, rhs: Expression): BinaryExpression = new BinaryExpression(LOGCIAL_OR, lhs, rhs)
    def tLt(lhs: Expression, rhs: Expression): BinaryExpression = new BinaryExpression(LESS_THAN, lhs, rhs)
    def tLte(lhs: Expression, rhs: Expression): BinaryExpression = new BinaryExpression(LESS_THAN_OR_EQUAL, lhs, rhs)
    def tGt(lhs: Expression, rhs: Expression): BinaryExpression = new BinaryExpression(GREATER_THAN, lhs, rhs)
    def tGte(lhs: Expression, rhs: Expression): BinaryExpression = new BinaryExpression(GREATER_THAN_OR_EQUAL, lhs, rhs)
    def tEqual(lhs: Expression, rhs: Expression): BinaryExpression = new BinaryExpression(EQUAL, lhs, rhs)
    def tNotEqual(lhs: Expression, rhs: Expression): BinaryExpression = new BinaryExpression(NOT_EQUAL, lhs, rhs)

    // Assignment
    def tAssign(variableName: String, newValue: Expression): AssignmentExpression = new AssignmentExpression(variableName, newValue)

    // Control structures
    def tBlock(elements: Expression*): Block = new Block(elements.toList)
    def tLet(variableName: String, init: Expression, body: Block): LetExpression = new LetExpression(variableName, init, body)
    def tLet(variableName: String, init: Expression, bodyFactory: String => Block): LetExpression = {
      val body: Block = bodyFactory.apply(variableName)
      new LetExpression(variableName, init, body)
    }
    def tIf(tCondition: Expression, tThen: Expression, tElse: Expression): IfExpression = {
      new IfExpression(tCondition, Block(tThen), Block(tElse))
    }
    def tWhile(tCondition: Expression, tBody: Expression*): WhileExpression = new WhileExpression(tCondition, tBody.toList)

    // Function definitions
    def tDef(name: String, args: List[String], body: Block): DefFunction = new DefFunction(name, args, body)

    // Function invocations
    def tCall(name: String, parameters: List[Expression]): FunctionCall = new FunctionCall(name, parameters)
    def tCall(name: String, parameters: Expression*): FunctionCall = {
      new FunctionCall(name, parameters.toList)
    }

  }

}
