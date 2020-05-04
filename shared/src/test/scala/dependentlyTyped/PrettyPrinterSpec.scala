package dependentlyTyped


import org.scalatest._
import org.scalatest.matchers.should.Matchers

class PrettyPrinterSpec extends FlatSpec with Matchers {

  it should "format function types" in {
    simpleCheck("a -> b")
    simpleCheck("a -> b -> c")
    simpleCheck("∀ (a :: *) . a -> b")
    simpleCheck("∀ (b :: Nat) (a :: Bool) . b a")
  }

  ignore should "format function types 2" in {
    val term = Parser.parseTerm("∀ (a :: *) (b :: a) . b")

    print(term)
    simpleCheck("∀ (a :: *) (b :: a) . b")
  }

  private def simpleCheck(s: String) = {
    val term = Parser.parseTerm(s)

    val prettyPrinted = PrettyPrinter.prettyPrint(term)

    prettyPrinted shouldEqual s
    Parser.parseTerm(prettyPrinted) shouldEqual term
  }

  it should "work for nested lambdas" in {
    val term = Parser.parseTerm("(λf x -> f (f x)) :: a")

    val prettyPrinted = PrettyPrinter.prettyPrint(term)

    prettyPrinted shouldEqual "((λb a -> b (b a)) :: a)"
    Parser.parseTerm(prettyPrinted) shouldEqual term
  }

}
