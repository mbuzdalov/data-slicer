package ru.ifmo.ds.srv

import java.nio.file.Paths

import scala.io.Source
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.IMain

object Main {
  class Variables {
    private var stateFileNameVar: Option[String] = None
    private var singleStepVar: Option[Boolean] = None
    private var dataRootVar: Option[String] = None
    private var phasesVar: Option[Seq[Phase]] = None

    def stateFileName: String = stateFileNameVar.getOrElse(throw new IllegalStateException("'stateFileName' is not defined"))
    def singleStep: Boolean = singleStepVar.getOrElse(throw new IllegalStateException("'singleStep' is not defined"))
    def dataRoot: String = dataRootVar.getOrElse(throw new IllegalArgumentException("'dataRoot' is not defined"))
    def phases: Seq[Phase] = phasesVar.getOrElse(throw new IllegalArgumentException("'phases' is not defined"))


    def stateFileName_=(value: String): Unit = stateFileNameVar = Some(value)
    def singleStep_=(value: Boolean): Unit = singleStepVar = Some(value)
    def dataRoot_=(value: String): Unit = dataRootVar = Some(value)
    def phases_=(value: Seq[Phase]): Unit = phasesVar = Some(value)
  }

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      sys.error("Usage: ru.ifmo.ds.srv.Main <config-class>")
    } else {
      val cfgFile = Paths.get(args(0))
      val root = cfgFile.getParent

      val settings = new Settings()
      settings.embeddedDefaults[Main.type]
      settings.usejavacp.value = true

      val vars = new Variables

      val loop = new IMain(settings)
      loop.interpret("import ru.ifmo.ds.srv._")
      loop.bind("vars", vars)
      val source = Source.fromFile(cfgFile.toFile)
      val lines = source.getLines().mkString("\n")
      source.close()
      loop.interpret(lines)

      PhaseExecutor.run(vars.phases, root.resolve(vars.dataRoot), vars.stateFileName, vars.singleStep)
    }
  }
}
