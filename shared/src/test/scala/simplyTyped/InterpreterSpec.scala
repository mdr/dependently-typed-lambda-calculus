package simplyTyped

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterpreterSpec extends AnyFlatSpec with Matchers {

  "Evaluating identity" should "work" in {
    val term = Parser.parseTerm("((λx -> x) :: a -> a) y")

    val result = Evaluator.eval(term)

    Quoter.quote(result) shouldEqual Term.Inf(Parser.parseTerm("y"))
  }

  "Typechecking an identity expression" should "work" in {
    val term = Parser.parseTerm("((λx -> x) :: a -> a) y")
    val Γ = Context.empty
      .withGlobalKind("a", *)
      .withGlobalType("y", FreeType("a"))

    val Right(typ: Type) = TypeChecker.inferType(term, Γ)

    typ shouldEqual Parser.parseType("a")
  }

  "Church numerals" should "work" in {
    val session =
      for {
        _ <- Interpreter.interpret("assume (a :: *)")
        _ <- Interpreter.interpret("let zero = (λf x -> x) :: (a -> a) -> (a -> a)")
        _ <- Interpreter.interpret("let one = (λf x -> f x) :: (a -> a) -> (a -> a)")
        _ <- Interpreter.interpret("let succ = (λn f x -> f (n f x)) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))")
        _ <- Interpreter.interpret("let two = succ one")
        _ <- Interpreter.interpret("let three = succ two")
        _ <- Interpreter.interpret("let four = succ three")
        _ <- Interpreter.interpret("let five = succ four")
        _ <- Interpreter.interpret("let six = succ five")
        _ <- Interpreter.interpret("let add = (λm n f x -> m f (n f x)) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))")
        _ <- Interpreter.interpret("let mult = (λm n f x -> m (n f) x) :: ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a)) -> ((a -> a) -> (a -> a))")
        twoPlusFour <- Interpreter.eval("add two four")
        threeTimesTwo <- Interpreter.eval("mult three two")
      } yield twoPlusFour -> threeTimesTwo

    val (twoPlusFour, threeTimesTwo) = session.run()

    twoPlusFour shouldEqual threeTimesTwo
  }

  "Parsing exception bug" should "not happen" in {
    val Left(_) = Parser.parseTermSafe("(λx -> f x) ::")
  }

}
