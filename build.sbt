lazy val commonSettings = Seq(
  organization := "ru.ifmo",
  scalaVersion := "2.12.6",
  libraryDependencies += scalaTest,
  fork := true
)

lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.0.5" % Test
lazy val gson       = "com.google.code.gson" % "gson" % "2.8.1"
lazy val apacheMath = "org.apache.commons" % "commons-math3" % "3.6.1"
lazy val xChart     = "org.knowm.xchart" % "xchart" % "3.5.0"

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name    := "data-slicer",
            version := "0.0.0")
  .dependsOn(core, cli)
  .aggregate(core, cli)

lazy val core = project
  .in(file("core"))
  .settings(commonSettings :_*)
  .settings(
    name    := "data-slicer-core",
    version := "0.0.0",
    libraryDependencies ++= Seq(gson, apacheMath))

lazy val cli = project
  .in(file("cli"))
  .dependsOn(core)
  .settings(commonSettings :_*)
  .settings(
    name    := "data-slicer-cli",
    version := "0.0.0")
