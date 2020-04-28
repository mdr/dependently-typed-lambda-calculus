package simplyTyped

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object InstructionsTable {
  val instructionsTable =
    ScalaComponent.static("No args")(
      <.table(^.`class` := "table table-bordered",
        <.tr(
          <.td("Assume a type variable"),
          <.td(<.code("assume a :: *")),
        ),
        <.tr(
          <.td("Assume a variable of a given type"),
          <.td(<.code("assume x :: a")),
        ),
        <.tr(
          <.td("Define a variable"),
          <.td(<.code("let two = (\\f x -> f (f x)) :: (a -> a) -> (a -> a)")),
        ),
        <.tr(
          <.td("Evaluate an expression"),
          <.td(<.code("two (\\x -> x)")),
        )
      )
    )
}
