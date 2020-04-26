package simplyTyped

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

object BindingsTable {

  val bindingsTable =
    ScalaComponent.builder[InterpreterState]("BindingsTable")
      .render_P(interpreterState => {
        val rows = interpreterState.Γ.infoByName.toSeq.sortBy(_._1.toString).collect { case (Name.Global(name), info) =>
          <.tr(
            <.td(name),
            <.td(interpreterState.letBindings.getOrElse(name, "").toString),
            <.td(info match {
              case HasKind(*) => "*"
              case HasType(typ) => typ.toString
            })
          )
        }
        <.table(^.`class` := "table",
          <.thead(
            <.tr(
              <.th(^.scope := "col", "Name"),
              <.th(^.scope := "col", "Value"),
              <.th(^.scope := "col", "Type")
            )
          ),
          <.tbody(rows.toTagMod)
        )
      })
      .build

}