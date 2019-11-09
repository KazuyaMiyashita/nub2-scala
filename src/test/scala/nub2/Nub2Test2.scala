package nub2

import org.scalatest._

import nub2.Ast._
import nub2.Ast.Factory._

class Nub2Test2 extends FunSuite with Matchers {

  def eval(input: Expression): Any = {
    return new Evaluator().eval(Block(input))
  }

  def eval(input: Block): Any = {
    return new Evaluator().eval(input)
  }

  test("testDefineLocalFunction") {
    val evaluated = eval(
      tBlock(
        tLet(
          "num",
          tInt(42),
          num => tBlock(
            tDef(
              "add",
              List("x"),
              tBlock(tAdd(tId("x"), tId(num)))
            ),
            tCall("add", tInt(1))
          )
        )
      )
    )
    val assumption = 43

    evaluated shouldEqual assumption
  }

}
