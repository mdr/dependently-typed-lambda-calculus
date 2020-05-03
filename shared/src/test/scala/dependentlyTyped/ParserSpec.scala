package dependentlyTyped

import org.scalatest._
import org.scalatest.matchers.should.Matchers

class ParserSpec extends FlatSpec with Matchers {

  "Parsing" should "work" in {
    Parser.parseTerm("forall (x :: *) (y :: Nat) (z :: Vec x y) (a :: Fin y) . x")
    Parser.parseTerm("""forall (m :: Nat -> *) . m 0 -> m 1 ->
                       |     (forall n :: Nat . m (Succ n) -> m (Succ (Succ n))) ->
                       |     forall (n :: Nat) . m n""".stripMargin)
  }

  "thing" should "work" in {
    val term = Parser.parseTerm("""forall (a :: *) . a -> a""")
    import Term._
    term shouldEqual Pi(Inf(*),Inf(Pi(Inf(BoundVariable(0)),Inf(BoundVariable(1)))))
  }

}