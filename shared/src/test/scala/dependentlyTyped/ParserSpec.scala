package dependentlyTyped

import org.scalatest._
import org.scalatest.matchers.should.Matchers

class ParserSpec extends FlatSpec with Matchers {

  "Parsing" should "work" in {
    Parser.parseTerm("forall (x :: *) (y :: Nat) (z :: Vec x y) (a :: Fin y) . x")
    Parser.parseTerm("""(\ a b x y -> x) :: forall (a :: *) (b :: *) . a -> b -> a""")
    Parser.parseTerm("""f (\a -> b -> c)""")
    Parser.parseTerm(
      """
        |  natElim
        |    ( \ _ -> Nat -> Bool )
        |    ( natElim
        |        ( \ _ -> Bool )
        |        True
        |        ( \ n' _ -> False ) )
        |    ( \ m' rec_m' -> natElim
        |                       ( \ _ -> Bool )
        |                       False
        |                       ( \ n' _ -> rec_m' n' ))""".stripMargin)
    Parser.parseTerm(
      """  ( \ a b f -> eqElim a
        |                 (\ x y eq_x_y -> Eq b (f x) (f y))
        |                 (\ x -> Refl b (f x)) )
        |  :: forall (a :: *) (b :: *) (f :: a -> b) (x :: a) (y :: a) .
        |     Eq a x y -> Eq b (f x) (f y)""".stripMargin)
    Parser.parseTerm(
      """  (\ a -> vecElim a ( \ n v -> Fin n -> a )
        |                    ( \ f -> voidElim (\ _ -> a) f )
        |                    ( \ n' v vs rec_n' f_SuccN' ->
        |                      finElim ( \ n _ -> Eq Nat n (Succ n') -> a )
        |                              ( \ n e -> v )
        |                              ( \ n f_N _ eq_SuccN_SuccN' ->
        |                                rec_n' (eqElim Nat
        |                                               (\ x y e -> Fin x -> Fin y)
        |                                               (\ _ f -> f)
        |                                               n n'
        |                                               (leibniz Nat Nat pred
        |                                                        (Succ n) (Succ n') eq_SuccN_SuccN')
        |                                               f_N))
        |                              (Succ n')
        |                              f_SuccN'
        |                              (Refl Nat (Succ n'))))
        |  :: forall (a :: *) (n :: Nat) . Vec a n -> Fin n -> a""".stripMargin
    )
    Parser.parseTerm(""" a  :: n -> forall (b :: *) . b""".stripMargin)
    Parser.parseTerm(
      """( \ m mf mt -> finElim ( nat2Elim (\ n -> Fin n -> *)
        |                                    (\ _ -> Unit) (\ _ -> Unit)
        |                                    (\ x -> m x)
        |                                    (\ _ _ _ -> Unit) )
        |                         ( nat1Elim ( \ n -> nat1Elim (\ n -> Fin (Succ n) -> *)
        |                                                      (\ _ -> Unit)
        |                                                      (\ x -> m x)
        |                                                      (\ _ _ _ -> Unit)
        |                                                      n (FZero n))
        |                                    U mf (\ _ _ -> U) )
        |                         ( \ n f _ -> finElim ( \ n f -> nat1Elim (\ n -> Fin (Succ n) -> *)
        |                                                                  (\ _ -> Unit)
        |                                                                  (\ x -> m x)
        |                                                                  (\ _ _ _ -> Unit)
        |                                                                  n (FSucc n f) )
        |                                              ( natElim
        |                                                  ( \ n -> natElim
        |                                                             (\ n -> Fin (Succ (Succ n)) -> *)
        |                                                             (\ x -> m x)
        |                                                             (\ _ _ _ -> Unit)
        |                                                             n (FSucc (Succ n) (FZero n)) )
        |                                                  mt (\ _ _ -> U) )
        |                                              ( \ n f _ -> finElim
        |                                                             (\ n f -> natElim
        |                                                                         (\ n -> Fin (Succ (Succ n)) -> *)
        |                                                                         (\ x -> m x)
        |                                                                         (\ _ _ _ -> Unit)
        |                                                                         n (FSucc (Succ n) (FSucc n f)))
        |                                                             (\ _ -> U)
        |                                                             (\ _ _ _ -> U)
        |                                                             n f )
        |                                              n f )
        |                         two )
        |  :: forall (m :: Bool -> *) . m False -> m True -> forall (b :: Bool) . m b""".stripMargin)
  }

  "thing" should "work" in {
    val term = Parser.parseTerm("""forall (a :: *) . a -> a""")
    import Term._
    term shouldEqual Pi(Inf(*), Inf(Pi(Inf(BoundVariable(0)), Inf(BoundVariable(1)))))
  }

}