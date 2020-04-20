package simplyTyped

import org.scalatest._
import org.scalatest.matchers.should.Matchers

class EvaluatorSpec extends FlatSpec with Matchers {

  "Evaluating identity" should "work" in {
    val term = Parser.parse("((λx -> x) :: a -> a) y")

    val result = Evaluator.eval(term)

    Quoter.quote(result) shouldEqual Term.Inf(Parser.parse("y"))
  }

  "Typechecking an identity expression" should "work" in {
    val term = Parser.parse("((λx -> x) :: a -> a) y")
    val Γ = Context.empty
      .withGlobalKind("a", *)
      .withGlobalType("y", FreeType("a"))

    val Right(typ: Type) = TypeChecker.inferType(term, Γ)

    typ shouldEqual Parser.parseType("a")
  }

  "Zero" should "work" in {
    val zero = Parser.parse("(λf x -> f x) :: (a -> a) -> (a -> a)")
    val one =  Parser.parse("(λf x -> f (f x)) :: (a -> a) -> (a -> a)")
    val succ = Parser.parse("(λn f x -> f (n f x)) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))")
    val resultZero = Evaluator.eval(zero)
    val resultOne = Evaluator.eval(one)
    val resultSucc = Evaluator.eval(succ)

    val Γ = Context.empty
      .withGlobalKind("a", *)
    val Right(_) = TypeChecker.inferType(zero, Γ)
    val Right(_) = TypeChecker.inferType(one, Γ)
    val Right(_) = TypeChecker.inferType(succ, Γ)

    val succZero = Parser.parse("((λn f x -> f (n f x)) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))) (λf x -> f x)")
    val resultSuccZero = Evaluator.eval(succZero)

    val succOne = Parser.parse("((λn f x -> f (n f x)) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))) (λf x -> f (f x))")
    val resultSuccOne = Evaluator.eval(succOne)

    val env = Environment.empty
      .extendWith("zero", resultZero)
      .extendWith("one", resultOne)
      .extendWith("succ", resultSucc)

    val succOneAgain = Parser.parse("succ one")
    val resultSuccOneAgain = Evaluator.eval(succOneAgain, env)

    println(resultSuccZero)
    println(resultSuccOne)
    println(resultSuccOneAgain)
    Quoter.quote(resultSuccZero) shouldEqual Quoter.quote(resultOne)
  }

}
