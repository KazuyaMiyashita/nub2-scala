package nub2

import org.scalatest._

import nub2.Ast._
import nub2.Ast.Factory._

class Nub2Test extends FunSuite with Matchers {

  def eval(input: Expression): Any = {
    return new Evaluator().eval(Block(input))
  }

  test("testStringLiteral") {
    val evaluated  = eval(tString("Hello, World!"))
    val assumption = "Hello, World!"

    evaluated shouldEqual assumption
  }

  test("testPrintln") {
    val evaluated  = eval(tString("Hello, World!"))
    val assumption = "Hello, World!"

    evaluated shouldEqual assumption
  }

  test("test1Plus1Is2") {
    val evaluated  = eval(tAdd(tInt(1), tInt(1)))
    val assumption = 2

    evaluated shouldEqual assumption
  }

  test("test1Minus1Is0") {
    val evaluated  = eval(tSubtract(tInt(1), tInt(1)))
    val assumption = 0

    evaluated shouldEqual assumption
  }

  test("test2Mul2Is4") {
    val evaluated  = eval(tMultiply(tInt(2), tInt(2)))
    val assumption = 4

    evaluated shouldEqual assumption
  }

  test("test6Div2Is3") {
    val evaluated  = eval(tDivide(tInt(6), tInt(2)))
    val assumption = 3

    evaluated shouldEqual assumption
  }

  test("test1Lt2") {
    val evaluated  = eval(tLt(tInt(1), tInt(2)))
    val assumption = true

    evaluated shouldEqual assumption
  }

  test("test2Lte2") {
    val evaluated  = eval(tLte(tInt(2), tInt(2)))
    val assumption = true

    evaluated shouldEqual assumption
  }

  test("test2Gt1") {
    val evaluated  = eval(tGt(tInt(2), tInt(1)))
    val assumption = true

    evaluated shouldEqual assumption
  }

  test("test2Gte2") {
    val evaluated  = eval(tGte(tInt(2), tInt(2)))
    val assumption = true

    evaluated shouldEqual assumption
  }

  test("testTrueAndTrueIsTrue") {
    val evaluated  = eval(tAnd(tBoolean(true), tBoolean(true)))
    val assumption = true

    evaluated shouldEqual assumption
  }

  test("testTrueAndFalseIsFalse") {
    val evaluated  = eval(tAnd(tBoolean(true), tBoolean(false)))
    val assumption = false

    evaluated shouldEqual assumption
  }

  test("testTrueOrFalseIsTrue") {
    val evaluated  = eval(tOr(tBoolean(true), tBoolean(false)))
    val assumption = true

    evaluated shouldEqual assumption
  }

  test("testFalseOrFalseIsFalse") {
    val evaluated  = eval(tOr(tBoolean(false), tBoolean(false)))
    val assumption = false

    evaluated shouldEqual assumption
  }

  test("testLetX10") {
    val evaluated = eval(
      tBlock(
        tLet(
          "x",
          tInt(10),
          x => tBlock(tId(x))
        )
      )
    )
    val assumption = 10

    evaluated shouldEqual assumption
  }

  test("testAssignmentX20") {
    val evaluated = eval(
      tBlock(
        tLet(
          "x",
          tInt(10),
          x =>
            tBlock(
              tAssign(x, tInt(20)),
              tId(x)
            )
        )
      )
    )
    val assumption = 20

    evaluated shouldEqual assumption
  }

  test("testWhileLt10") {
    val evaluated = eval(
      tBlock(
        tLet(
          "x",
          new IntLiteral(0),
          x =>
            tBlock(
              tWhile(
                tLt(new Id(x), tInt(10)),
                tAssign(
                  x,
                  tAdd(tId(x), tInt(1))
                )
              ),
              tId("x")
            )
        )
      )
    )
    val assumption = 10

    evaluated shouldEqual assumption
  }

  test("testIf1Lt2") {
    val evaluated = eval(
      tIf(
        tLt(tInt(1), tInt(2)),
        tString("1  < 2"),
        tString("1 >= 2")
      )
    )
    val assumption = "1 < 2"

    evaluated shouldEqual assumption
  }

  test("testUserDefinedAddFuntion") {
    val evaluated = eval(
      tBlock(
        tDef(
          "add",
          List("x", "y"),
          tBlock(tAdd(tId("x"), tId("y")))
        ),
        tCall("add", tInt(1), tInt(2))
      )
    )
    val assumption = 3

    evaluated shouldEqual assumption
  }

}
