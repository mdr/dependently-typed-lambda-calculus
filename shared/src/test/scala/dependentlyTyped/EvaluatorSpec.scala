package dependentlyTyped

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class EvaluatorSpec extends AnyFlatSpec with Matchers {

  "Stuff" should "work" in {
    check("Succ 2", "3")

    check("id Nat 1", "1")
    check("const Nat Nat 1 2", "1")
    check("plus 3 4", "7")
    check("pred 0", "0")
    check("pred 2", "1")
    check("inc 2", "3")

  }

  private def check(expression: String, expected: String) = eval(expected) shouldEqual eval(expression)

  private def eval(line: String): Value = {
    val (_, Right(InterpreterResult.Evaluated(_, value, _, _))) = Interpreter.interpret(line)(InterpreterState.prelude)
    value
  }
}