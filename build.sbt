name := "dependently-typed-lambda-calculus"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"

//libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"
//libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "1.6.0"
//libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2"
//libraryDependencies += "org.scalatest" %%% "scalatest" % "3.1.1" % "test"
//
//enablePlugins(ScalaJSPlugin)
//enablePlugins(ScalaJSBundlerPlugin)
//
//npmDependencies in Compile ++= Seq(
//  "react" -> "16.7.0",
//  "react-dom" -> "16.7.0")
//
//scalaJSUseMainModuleInitializer := true