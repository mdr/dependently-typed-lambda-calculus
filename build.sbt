name := "dependently-typed-lambda-calculus"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"
libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "1.6.0"
libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2"

enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)

npmDependencies in Compile ++= Seq(
  "react" -> "16.7.0",
  "react-dom" -> "16.7.0")

scalaJSUseMainModuleInitializer := true