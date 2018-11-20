package ru.ifmo.ds.srv

import java.nio.file.{Files, Path}
import java.util.Properties

object PhaseExecutor {
  def run(phases: Seq[Phase], projectRoot: Path, phaseRoot: Path, stateFileName: String, singleStep: Boolean): Unit = {
    def processPhase(index: Int, props: Properties, propsChanged: Boolean, curr: Path, prev: Option[Path]): (Boolean, Option[Throwable]) = {
      if (index >= phases.size) (propsChanged, None) else {
        val phase = phases(index)
        val completeKey = phase.key + ".complete"
        val printPrefix = s"Path ${phaseRoot.relativize(curr)}, phase ${phase.key}"
        if (props.getProperty(completeKey, "false") != "true") {
          try {
            println(s"$printPrefix: running...")
            phase.execute(projectRoot, curr, prev)
            println(s"$printPrefix: complete!")
            props.setProperty(completeKey, "true")
            if (singleStep) (true, None) else processPhase(index + 1, props, propsChanged = true, curr, prev)
          } catch {
            case th: Throwable => (propsChanged, Some(th))
          }
        } else {
          println(s"$printPrefix: already complete")
          processPhase(index + 1, props, propsChanged, curr, prev)
        }
      }
    }

    val workingSet = Files.list(phaseRoot)
      .filter(f => Files.exists(f.resolve(stateFileName)))
      .toArray[Path](Array.ofDim[Path])
      .sorted
    val liftedWorkingSet = workingSet.lift
    def processIndex(index: Int): Unit = if (index < workingSet.length) {
      val curr = workingSet(index)
      val prev = liftedWorkingSet(index - 1)
      val statePath = curr.resolve(stateFileName)
      val stateReader = Files.newBufferedReader(statePath)
      val props = new Properties()
      props.load(stateReader)
      stateReader.close()

      val (changed, optionalException) = processPhase(0, props, propsChanged = false, curr, prev)
      if (changed) {
        val stateWriter = Files.newBufferedWriter(statePath)
        props.store(stateWriter, null)
        stateWriter.close()
      }

      optionalException.foreach(ex => throw ex)
      processIndex(index + 1)
    }
    processIndex(0)
  }
}
