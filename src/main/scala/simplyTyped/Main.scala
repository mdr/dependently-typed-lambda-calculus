package simplyTyped

import org.scalajs.dom.document
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scala.util.control.NonFatal

case class State(input: String = "")

class Backend($: BackendScope[Unit, State]) {

  def onChange(e: ReactEventFromInput) = {
    val newValue = e.target.value
    $.modState(_.copy(input = newValue))
  }

  def render(state: State) = {
    val termString =
      try {
        val term = Parser.parse(state.input)
        val result = Evaluator.eval(term)
        term.toString -> result.toString
      } catch {
        case NonFatal(e) => e.getMessage
      }
    <.div(^.`class` := "container",
      <.h1("Simply-Typed Lambda Calculus"),
      <.div(
        <.input(^.`class` := "form-control", ^.onChange ==> onChange, ^.value := state.input)),
      <.div(s"${termString}")
    )
  }
}

object Main {

  val App2 =
    ScalaComponent.builder[Unit]("App")
      .initialState(State())
      .renderBackend[Backend]
      .build

  val App =
    ScalaComponent.builder[Unit]("App")
      .renderStatic(<.div("Hello!"))
      .build

  def main(args: Array[String]): Unit = {
    val target = document.getElementById("root")
    App2().renderIntoDOM(target)
  }

}

