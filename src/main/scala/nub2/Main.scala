package nub2

import nub2.Ast._
import nub2.Ast.Factory._

object Main extends App {

  val evaluator: Evaluator = new Evaluator
  evaluator.eval(
    tBlock(
      tPrintln(tString("Hello, Workd"))
    )
  )

}
