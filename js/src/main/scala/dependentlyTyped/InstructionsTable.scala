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
            <.td(<.code("""let two = (\a f x -> f (f x)) :: forall (a :: *) . (a -> a) -> (a -> a)""")),
          ),
          <.tr(
            <.td("Evaluate an expression"),
            <.td(<.code("""two (\x -> x)""")),
          )
        ),
      )
    )
}
