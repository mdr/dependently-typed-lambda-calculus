package dependentlyTyped

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object InstructionsTable {
  val instructionsTable =
    ScalaComponent.static("No args")(
      <.table(^.`class` := "table table-bordered",
        <.tbody(
          <.tr(
            <.td("Assume a type variable or variable of a given type"),
            <.td(<.code("assume (String :: *) (s :: String)")),
          ),
          <.tr(
            <.td("Define a variable"),
            <.td(<.code("""let multiply = natElim (\_ -> Nat -> Nat) (\n -> 0) (\p rec n -> plus n (rec n))""")),
          ),
          <.tr(
            <.td("Evaluate an expression"),
            <.td(<.code("""multiply 2 3""")),
          ),
          <.tr(
            <.td("Prove that for any natural numbers n and m, n + m = m + n"),
            <.td(<.code("""let plus_comm = natElim (\n -> forall (m :: Nat) . Eq Nat (plus n m) (plus m n)) (\m -> symm Nat (plus m 0) m (pNPlus0isN m)) (\n p m -> tran Nat (Succ (plus n m)) (Succ (plus m n)) (plus m (Succ n)) (leibniz Nat Nat Succ (plus n m) (plus m n) (p m)) ((natElim (\n -> forall (m :: Nat) . Eq Nat (Succ (plus n m)) (plus n (Succ m))) (\m -> Refl Nat (Succ m)) (\n p m-> leibniz Nat Nat Succ (Succ (plus n m)) (plus n (Succ m)) (p m))) m n)) :: forall (n :: Nat) (m :: Nat) . Eq Nat (plus n m) (plus m n)""")),
          )
        ),
      )
    )
}
