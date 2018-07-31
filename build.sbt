val theScalaVersion = "2.12.6"

lazy val commonSettings = Seq(
  organization := "ru.ifmo",
  version := "0.0.0",
  scalaVersion := theScalaVersion,
  libraryDependencies += scalaTest,
  fork := true
)

lazy val scalaCompiler = "org.scala-lang" % "scala-compiler" % theScalaVersion
lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.0.5" % Test
lazy val gson       = "com.google.code.gson" % "gson" % "2.8.1"
lazy val apacheMath = "org.apache.commons" % "commons-math3" % "3.6.1"
lazy val xChart     = "org.knowm.xchart" % "xchart" % "3.5.0"

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "data-slicer")
  .dependsOn(core, cli)
  .aggregate(core, cli)

lazy val core = project
  .in(file("core"))
  .settings(commonSettings :_*)
  .settings(
    name    := "data-slicer-core",
    libraryDependencies ++= Seq(gson, apacheMath))

lazy val cli = project
  .in(file("cli"))
  .dependsOn(core)
  .settings(commonSettings :_*)
  .settings(
    name    := "data-slicer-cli",
    libraryDependencies += scalaCompiler)
