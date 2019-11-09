package nub2

class VariableChecker extends Ast.ExpressionVisitor[Any] {

  override def visitBinaryExpression(node: Ast.BinaryExpression): Any = {
    null
  }

  override def visitIntLiteral(node: Ast.IntLiteral): Any = {
    null
  }

  override def visitBooleanLiteral(node: Ast.BooleanLiteral): Any = {
    null
  }

  override def visitStringLiteral(node: Ast.StringLiteral): Any = {
    null
  }

  override def visitLetExpression(node: Ast.LetExpression): Any = {
    null
  }

  override def visitId(node: Ast.Id): Any = {
    null
  }

  override def visitBlock(node: Ast.Block): Any = {
    null
  }

  override def visitIfExpression(node: Ast.IfExpression): Any = {
    null
  }

  override def visitWhileExpression(node: Ast.WhileExpression): Any = {
    null
  }

  override def visitAssignmentExpression(node: Ast.AssignmentExpression): Any = {
    null
  }

  override def visitPrintlnExpression(node: Ast.PrintlnExpression): Any = {
    null
  }

  override def visitDefFunction(node: Ast.DefFunction): Any = {
    null
  }

  override def visitFunctionCall(node: Ast.FunctionCall): Any = {
    null
  }

  def checkVariable(program: Ast.Block): Ast.Block = {
    program
  }

}
