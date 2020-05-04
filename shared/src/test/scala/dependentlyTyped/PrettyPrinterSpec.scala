package dependentlyTyped


import org.scalatest._
import org.scalatest.matchers.should.Matchers

class PrettyPrinterSpec extends FlatSpec with Matchers {

  "Pretty printing" should "work for nested pis" in {
    val term = Parser.parseTerm("forall (a :: Nat) (b :: Bool) . a b")

    val prettyPrinted = PrettyPrinter.prettyPrint(term)

    prettyPrinted shouldEqual "(∀ (b :: Nat) (a :: Bool) . b a)"
    Parser.parseTerm(prettyPrinted) shouldEqual term
  }

  it should "work for nested lambdas" in {
    val term = Parser.parseTerm("(λf x -> f (f x)) :: a")

    val prettyPrinted = PrettyPrinter.prettyPrint(term)

    prettyPrinted shouldEqual "((λb a -> b (b a)) :: a)"
    Parser.parseTerm(prettyPrinted) shouldEqual term
  }

}
