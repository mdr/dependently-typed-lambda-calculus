package dependentlyTyped

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PrettyPrinterSpec extends AnyFlatSpec with Matchers {

  it should "format annotations" in {
    simpleCheck("a :: *")
    simpleCheck("42 :: ℕ")
    simpleCheck("(λa -> a) :: a -> a")
  }

  it should "format function types" in {
    simpleCheck("a -> b")
    simpleCheck("a -> *")
    simpleCheck("a -> b -> c")
    simpleCheck("∀ (a :: *) . a -> b")
    simpleCheck("∀ (a :: ℕ) (b :: Bool) . b a")
    simpleCheck("∀ (a :: *) (b :: a) . b")
    simpleCheck("∀ (a :: *) (b :: a -> a) . b")
  }

  it should "format applications" in {
    simpleCheck("a b")
    simpleCheck("a *")
    simpleCheck("a (b -> c)")
    simpleCheck("x (∀ (a :: *) . a)")
    simpleCheck("x ((λa -> a) :: b)")
    simpleCheck("a (b c)")

    simpleCheck("a b c")
    simpleCheck("(a -> b) c")
    simpleCheck("* c")
    simpleCheck("(∀ (a :: *) . a) c")
    simpleCheck("((λa -> a) :: b) c")
  }

  it should "work for nested lambdas" in {
    val term = Parser.parseTerm("(λf x -> f (f x)) :: a")

    val prettyPrinted = PrettyPrinter.prettyPrint(term)

    prettyPrinted shouldEqual "(λb a -> b (b a)) :: a"
    Parser.parseTerm(prettyPrinted) shouldEqual term
  }

  private def simpleCheck(s: String): Assertion = {
    val term = Parser.parseTerm(s)

    val prettyPrinted = PrettyPrinter.prettyPrint(term)

    prettyPrinted shouldEqual s
    Parser.parseTerm(prettyPrinted) shouldEqual term
  }

}
