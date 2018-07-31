package ru.ifmo.ds.cli

import java.io.PrintStream

import ru.ifmo.ds.Main

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop

object Repl extends Main.Module {
  override def name: String = "repl"

  override def apply(args: Array[String]): Unit = {
    val settings = new Settings()
    settings.embeddedDefaults[Repl.type]
    settings.usejavacp.value = true
    settings.sourceReader.value
    val loop = new ILoop() {
      override def createInterpreter(): Unit = {
        super.createInterpreter()
        intp.interpret("import ru.ifmo.ds._")
        if (args.length > 0) {
          intp.interpret(s"val inputFile = new java.io.File(${'"' + args(0) + '"'})")
          intp.interpret("val database = io.Json.loadFromFile(inputFile)")
        }
      }
    }
    loop.process(settings)
    loop.loop()
  }

  override def printUsage(out: PrintStream): Unit = {
    out.println(s"    $name <filename>: run a REPL on the database <filename>.")
  }
}
