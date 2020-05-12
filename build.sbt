val theScalaVersion = "2.13.2"

lazy val commonSettings = Seq(
  organization := "ru.ifmo",
  version := "0.0.0",
  scalaVersion := theScalaVersion,
  scalacOptions ++= Seq("-deprecation", "-feature", "-opt-warnings:_", "-unchecked"),
  libraryDependencies += scalaTest,
  fork := true
)

lazy val scalaCompiler = "org.scala-lang" % "scala-compiler" % theScalaVersion

lazy val gson       = "com.google.code.gson" % "gson" % "2.8.6"
lazy val apacheMath = "org.apache.commons" % "commons-math3" % "3.6.1"
lazy val jFreeChart = "org.jfree" % "jfreechart" % "1.5.0"

lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.1.0" % Test
lazy val spire      = "org.typelevel" %% "spire" % "0.17.0-M1"

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "data-slicer")
  .dependsOn(core, cli, gui)
  .aggregate(core, cli, gui)

lazy val core = project
  .in(file("core"))
  .settings(commonSettings :_*)
  .settings(
    name    := "data-slicer-core",
    libraryDependencies ++= Seq(gson, apacheMath, spire))

lazy val cli = project
  .in(file("cli"))
  .dependsOn(core)
  .settings(commonSettings :_*)
  .settings(name := "data-slicer-cli")

lazy val gui = project
  .in(file("gui"))
  .dependsOn(core)
  .settings(commonSettings :_*)
  .settings(
    name    := "data-slicer-gui",
    libraryDependencies ++= Seq(jFreeChart))

lazy val service = project
  .in(file("service"))
  .dependsOn(core)
  .settings(commonSettings :_*)
  .settings(
    name := "data-slicer-service",
    libraryDependencies ++= Seq(scalaCompiler))
