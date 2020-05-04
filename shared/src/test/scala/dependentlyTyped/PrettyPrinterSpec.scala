package dependentlyTyped


import org.scalatest._
import org.scalatest.matchers.should.Matchers

class PrettyPrinterSpec extends FlatSpec with Matchers {

  "Pretty printing" should "work" in {
    val term = Parser.parseTerm("forall (a :: Nat) (b :: Bool) . a b")

    val prettyPrinted = PrettyPrinter.prettyPrint(term)
    
    prettyPrinted shouldEqual "(∀ (b :: Nat) (a :: Bool) . b a)"
    Parser.parseTerm(prettyPrinted) shouldEqual term
  }

}
