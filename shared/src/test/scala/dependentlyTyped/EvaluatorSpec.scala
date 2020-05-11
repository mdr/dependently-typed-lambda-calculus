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
    check("replicate 2 Nat 42", "Cons ℕ 1 42 (Cons ℕ 0 42 (Nil ℕ))")
    check("replicate' Nat 2 42", "Cons ℕ 1 42 (Cons ℕ 0 42 (Nil ℕ))")
    check("fromto 2", "Cons ℕ 1 1 (Cons ℕ 0 0 (Nil ℕ))")
    check("fromto 3", "Cons ℕ 2 2 (Cons ℕ 1 1 (Cons ℕ 0 0 (Nil ℕ)))")
    check("append Nat 2 (fromto 2) 3 (fromto 3)", "Cons ℕ 4 1 (Cons ℕ 3 0 (Cons ℕ 2 2 (Cons ℕ 1 1 (Cons ℕ 0 0 (Nil ℕ)))))")
  }

  private def check(expression: String, expected: String) = eval(expected) shouldEqual eval(expression)

  private def eval(line: String): Value = {
    val (_, Right(InterpreterResult.Evaluated(_, value, _, _))) = Interpreter.interpret(line)(InterpreterState.prelude)
    value
  }
}