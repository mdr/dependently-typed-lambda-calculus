package simplyTyped

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import simplyTyped.InterpreterResult.InterpreterOutcome

object InterpreterOutcomeView {

  val interpreterOutcomeView = ScalaComponent.builder[InterpreterOutcome]("interpreterOutcomeView")
    .render_P {
      case Left(error) =>
        <.div(^.`class` := "alert alert-danger", ^.role := "alert", "Error: ", <.code(error))
      case Right(InterpreterResult.Assume(assumptions, _)) =>
        <.div(^.`class` := "alert alert-info", ^.role := "alert",
          assumptions.map {
            case InterpreterResult.Assumption(name, HasKind(kind)) => <.code(s"$name :: $kind")
            case InterpreterResult.Assumption(name, HasType(typ)) => <.code(s"$name :: $typ")
          }.toTagMod)
      case Right(InterpreterResult.Evaluated(name, value, typ, _)) =>
        <.div(^.`class` := "alert alert-success", ^.role := "alert",
          <.code(s"$name :: $typ"),
          <.br(),
          <.code(s"$name = $value"))
    }
    .build


}
