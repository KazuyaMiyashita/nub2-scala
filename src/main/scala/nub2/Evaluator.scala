package nub2

import scala.collection.mutable

class Evaluator extends Ast.ExpressionVisitor[Any] {

  import Evaluator._

  private var environment: Environment            = Environment(null)
  private val functions: mutable.Map[String, Ast.DefFunction] = new mutable.HashMap

  private def asBoolean(value: Any): Boolean = {
    value.asInstanceOf[Boolean]
  }

  private def asInt(value: Any): Int = {
    value.asInstanceOf[Int]
  }

  override def visitBinaryExpression(node: Ast.BinaryExpression): Any = {
    import Ast.BinaryOperator._

    node.operator match {
      case ADD => {
        val lhs: Any = node.lhs.accept(this)
        val rhs: Any = node.rhs.accept(this)
        if (lhs.isInstanceOf[String] || rhs.isInstanceOf[String]) lhs.toString + rhs.toString
        else asInt(lhs) + asInt(rhs)
      }
      case SUBTRACT => {
        asInt(node.lhs.accept(this)) - asInt(node.rhs.accept(this))
      }
      case MULTIPLY => {
        asInt(node.lhs.accept(this)) * asInt(node.rhs.accept(this))
      }
      case DIVIDE => {
        asInt(node.lhs.accept(this)) / asInt(node.rhs.accept(this))
      }
      case LESS_THAN => {
        asInt((node.lhs.accept(this))) < asInt(node.rhs.accept(this))
      }
      case LESS_THAN_OR_EQUAL => {
        asInt((node.lhs.accept(this))) <= asInt(node.rhs.accept(this))
      }
      case GREATER_THAN => {
        asInt((node.lhs.accept(this))) > asInt(node.rhs.accept(this))
      }
      case GREATER_THAN_OR_EQUAL => {
        asInt((node.lhs.accept(this))) >= asInt(node.rhs.accept(this))
      }
      case EQUAL => {
        node.lhs.accept(this) == node.rhs.accept(this)
      }
      case NOT_EQUAL => {
        asInt((node.lhs.accept(this))) != asInt(node.rhs.accept(this))
        throw new NotImplementedException("equality operator " + node.operator.op)
      }
      case LOGICAL_AND => {
        asBoolean((node.lhs.accept(this))) && asBoolean(node.rhs.accept(this))
      }
      case LOGCIAL_OR => {
        asBoolean((node.lhs.accept(this))) || asBoolean(node.rhs.accept(this))
      }
    }
  }
  override def visitIntLiteral(node: Ast.IntLiteral): Any = {
    node.value
  }
  override def visitBooleanLiteral(node: Ast.BooleanLiteral): Any = {
    node.value
  }
  override def visitStringLiteral(node: Ast.StringLiteral): Any = {
    node.value
  }
  override def visitLetExpression(node: Ast.LetExpression): Any = {
    val value: Any = node.init.accept(this)
    if (environment.contains(node.variableName)) {
      throw new NubRuntimeException("variable " + node.variableName + " is already defined")
    }
    var backup: Environment = environment
    this.environment = new Environment(environment)
    environment.register(node.variableName, value)
    val result: Any = node.body.accept(this)
    this.environment = backup
    result
  }
  override def visitId(node: Ast.Id): Any = {
    val ret: Any = environment.find(node.name)
    if (ret == null) throw new NubRuntimeException(node.name + " is not defined")
    else ret
  }
  override def visitBlock(node: Ast.Block): Any = {
    var last: Any = 0
    node.expressions.foreach { e =>
      last = e.accept(this)
    }
    last
  }
  override def visitIfExpression(node: Ast.IfExpression): Any = {
    if (asBoolean(node.condition.accept(this))) node.thenClause.accept(this)
    else node.elseClause.accept(this)
  }
  override def visitWhileExpression(node: Ast.WhileExpression): Any = {
    while (asBoolean(node.condition.accept(this))) {
      node.body.foreach { exp =>
        exp.accept(this)
      }
    }
    ()
  }
  override def visitAssignmentExpression(node: Ast.AssignmentExpression): Any = {
    environment.findEnvironment(node.variableName) match {
      case Some(environmentDefinedVariable) => {
        val value: Any = node.expression.accept(this)
        environmentDefinedVariable.register(node.variableName, value)
      }
      case None => throw new NubRuntimeException("variable " + node.variableName + " is not defined")
    }
    ()
  }
  override def visitPrintlnExpression(node: Ast.PrintlnExpression): Any = {
    val value: Any = node.target.accept(this)
    System.out.println(value)
    value
  }
  override def visitDefFunction(node: Ast.DefFunction): Any = {
    // Nothing to be done
    null
  }
  override def visitFunctionCall(node: Ast.FunctionCall): Any = {
    val function = functions.get(node.name).getOrElse(null)
    if (function == null) throw new NubRuntimeException("function " + node.name + " is not defined")

    val localEnvironment = Environment(null)
    (function.args zip node.params).foreach { case (name, value) =>
      localEnvironment.register(name, value.accept(this))
    }
    val backup = environment
    this.environment = localEnvironment
    val result = visitBlock(function.body)
    this.environment = backup
    result
  }

  def eval(program: Ast.Block): Any = {
    var target: Ast.Block        = program
    val checker: VariableChecker = new VariableChecker
    target = checker.checkVariable(program)
    val typer: Typer = new Typer
    target = typer.typeCheck(target)
    target.expressions.foreach { top =>
      if (top.isInstanceOf[Ast.DefFunction]) {
        val f: Ast.DefFunction = top.asInstanceOf[Ast.DefFunction]
        functions.put(f.name, f)
      }
    }
    program.accept(this)
  }

}

object Evaluator {

  case class Environment(parent: Environment) {
    val mapping: mutable.Map[String, Any] = new mutable.HashMap

    def find(name: String): Any = {
      val value: Any = mapping.get(name).getOrElse(null)
      if (value == null && parent != null) parent.find(name)
      else value
    }

    def findEnvironment(name: String): Option[Environment] = {
      if (mapping.contains(name)) Some(this)
      else if (parent != null) parent.findEnvironment(name)
      else None
    }

    def contains(name: String): Boolean = {
      if (mapping.contains(name)) true
      else if (parent != null) parent.contains(name)
      else false
    }

    def register(name: String, value: Any): Any = {
      mapping.put(name, value).getOrElse(null)
    }
  }

}
