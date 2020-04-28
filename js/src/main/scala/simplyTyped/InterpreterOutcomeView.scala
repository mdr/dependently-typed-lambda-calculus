package simplyTyped

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import simplyTyped.InterpreterResult.InterpreterOutcome

object InterpreterOutcomeView {

  val interpreterOutcomeView = ScalaComponent.builder[InterpreterOutcome]("interpreterOutcomeView")
    .render_P {
      case Left(error) =>
        <.div(^.`class` := "alert alert-danger", ^.role := "alert", "Error: ", <.code(error))
      case Right(InterpreterResult.Assume(name, HasKind(kind))) =>
        <.div(^.`class` := "alert alert-info", ^.role := "alert", <.code(s"$name :: $kind"))
      case Right(InterpreterResult.Assume(name, HasType(typ))) =>
        <.div(^.`class` := "alert alert-info", ^.role := "alert", <.code(s"$name :: $typ"))
      case Right(InterpreterResult.Evaluated(name, value, typ)) =>
        <.div(^.`class` := "alert alert-success", ^.role := "alert",
          <.code(s"$name :: $typ"),
          <.br(),
          <.code(s"$name = $value"))
    }
    .build


}
