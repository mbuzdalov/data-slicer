package ru.ifmo.ds.srv

import java.io.IOException
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}
import java.util.Properties
import java.util.stream.Collectors

import scala.collection.JavaConverters._

import ru.ifmo.ds.Database
import ru.ifmo.ds.io.Json
import ru.ifmo.ds.ops.FindDifferences
import ru.ifmo.ds.ops.FindDifferences.DifferenceListener
import ru.ifmo.ds.stat.KolmogorovSmirnov

object Main {
  final val DataRoot = new PropertyKey("data.root")
  final val DataStateFilename = new PropertyKey("data.state.filename")
  final val DataSubdirectoryRaw = new PropertyKey("data.subdirectory.raw")
  final val DataSubdirectoryConsolidated = new PropertyKey("data.subdirectory.consolidated")
  final val ListOfAlgorithms = new PropertyKey("list.of.changed.algorithms")

  final val KeyValue = "primaryMetric.rawData"
  final val KeyCats = Seq("benchmark", "params.d", "params.f", "params.n")
  final val KeyAlgorithm = "params.algorithmId"
  final val BasicPValue = 1e-3

  private[this] var terminateOnPhaseCompletion = false

  class PropertyKey(val key: String) extends AnyVal
  private[this] implicit class PropertiesEx(val p: Properties) extends AnyVal {
    def apply(key: PropertyKey): String = p.getProperty(key.key)
  }
  private[this] class StageTermination(message: String) extends RuntimeException(message)

  private[this] def usage(): Nothing = {
    sys.error("Usage: ru.ifmo.ds.srv.Main <config-file>")
  }

  private[this] def setCompleteKey(p: Properties, key: String): Unit = {
    p.setProperty(key, "true")
    if (terminateOnPhaseCompletion) throw new StageTermination(key)
  }

  private[this] class CompareListener(p: Double) extends DifferenceListener {
    private[this] val differingAlgorithms = IndexedSeq.newBuilder[String]

    def result(): IndexedSeq[String] = differingAlgorithms.result()

    override def keyValuesDoNotMatch(slice: Map[String, Option[String]], key: String,
                                     onlyLeft: Set[Option[String]],
                                     onlyRight: Set[Option[String]]): Unit = {
      // It can be that the new commit features new algorithms which did not exist yet. These need to be added.
      // It can be that the new commit deletes some of the old algorithms. These need not to be added.

      if (key == KeyAlgorithm) {
        onlyRight.foreach(differingAlgorithms ++= _)
      }
    }

    override def kolmogorovSmirnovFailure(slice: Map[String, Option[String]],
                                          key: String, leftValues: Seq[String], rightValues: Seq[String],
                                          exception: Throwable): Unit = throw exception

    override def kolmogorovSmirnovResult(slice: Map[String, Option[String]],
                                         key: String, leftValues: Seq[Double], rightValues: Seq[Double],
                                         result: KolmogorovSmirnov.Result): Unit = {}

    override def sliceStatistics(slice: Map[String, Option[String]],
                                 key: String, statistics: Seq[KolmogorovSmirnov.Result]): Unit = {
      if (slice.keySet == Set(KeyAlgorithm) && slice(KeyAlgorithm).nonEmpty) {
        if (KolmogorovSmirnov.rankSumOnMultipleOutcomes(statistics) < p) {
          differingAlgorithms ++= slice(KeyAlgorithm)
        }
      }
    }
  }

  private[this] def runCompute(p: Properties, root: Path, curr: Path, phase: String): Unit = {
    val completeKey = phase + ".compute.complete"
    val currentPhaseOut = phase + ".json"
    val useKey = phase.substring(phase.lastIndexOf('.') + 1)
    val lOA = p(ListOfAlgorithms)
    if (p.getProperty(completeKey, "false") != "true") {
      val outputFile = curr.resolve(p(DataSubdirectoryRaw)).resolve(currentPhaseOut)
      val algorithmFile = curr.resolve(lOA)
      val algorithms = if (Files.exists(algorithmFile)) {
        Files.lines(curr.resolve(lOA)).collect(Collectors.joining(",", "--algo=", ""))
      } else ""
      val pb = new ProcessBuilder()
      pb.command("sbt",
                 "project benchmarking",
                 s"jmh:runMain ru.ifmo.nds.jmh.main.Minimal $algorithms --use=$useKey '--out=${outputFile.toAbsolutePath.toString}'")
      pb.inheritIO().directory(root.toFile)
      val exitCode = pb.start().waitFor()
      if (exitCode != 0) {
        throw new IOException("Exit code " + exitCode)
      }
      setCompleteKey(p, completeKey)
    }
  }

  private[this] def runMinimalMinCompare(p: Properties, curr: Path, prevOption: Option[Path]): Unit = {
    val completeKey = "phase.minimal.min.compare.complete"
    val currentPhaseOut = "minimal-min.json"
    val listOfAlgorithms = curr.resolve(p(ListOfAlgorithms))
    if (p.getProperty(completeKey, "false") != "true") {
      prevOption match {
        case Some(prev) =>
          val oldDB = Json.fromFile(prev.resolve(p(DataSubdirectoryConsolidated)).resolve(currentPhaseOut).toFile)
          val newDB = Json.fromFile(curr.resolve(p(DataSubdirectoryRaw)).resolve(currentPhaseOut).toFile)
          val commonAlgorithms = oldDB.valuesUnderKey(KeyAlgorithm).intersect(newDB.valuesUnderKey(KeyAlgorithm))
          val listener = new CompareListener(BasicPValue / commonAlgorithms.size)
          FindDifferences.traverse(oldDB, newDB, KeyAlgorithm +: KeyCats, KeyValue, listener)
          Files.write(listOfAlgorithms, listener.result().asJava, Charset.defaultCharset())
        case None =>
          // No previous runs detected. Need to write all algorithms to the file
          val file = curr.resolve(p(DataSubdirectoryRaw)).resolve(currentPhaseOut)
          val allAlgorithms = Json.fromFile(file.toFile).valuesUnderKey(KeyAlgorithm).flatMap(_.iterator).toIndexedSeq.sorted
          Files.write(listOfAlgorithms, allAlgorithms.asJava, Charset.defaultCharset())
      }
      setCompleteKey(p, completeKey)
    }
  }

  private[this] def runConsolidation(p: Properties, curr: Path, prevOption: Option[Path], phase: String): Unit = {
    val completeKey = phase + ".consolidate.complete"
    val currentPhaseOut = phase + ".json"
    if (p.getProperty(completeKey, "false") != "true") {
      prevOption match {
        case None =>
          // no previous runs - just copy a file over
          val src = curr.resolve(p(DataSubdirectoryRaw)).resolve(currentPhaseOut)
          val trg = curr.resolve(p(DataSubdirectoryConsolidated)).resolve(currentPhaseOut)
          Files.createLink(trg, src)
        case Some(prev) =>
          val oldDB = Json.fromFile(prev.resolve(p(DataSubdirectoryConsolidated)).resolve(currentPhaseOut).toFile)
          val newDB = Json.fromFile(curr.resolve(p(DataSubdirectoryRaw)).resolve(currentPhaseOut).toFile)
          val differingAlgorithms = Files.readAllLines(curr.resolve(p(ListOfAlgorithms))).asScala.toSet
          val oldDBFiltered = oldDB.filter(e => e.contains(KeyAlgorithm) && !differingAlgorithms.contains(e(KeyAlgorithm)))
          val merged = Database.merge(oldDBFiltered, newDB)
          val trg = curr.resolve(p(DataSubdirectoryConsolidated)).resolve(currentPhaseOut)
          Json.writeToFile(merged, trg.toFile)
      }
      setCompleteKey(p, completeKey)
    }
  }

  private[this] def executeOne(p: Properties, root: Path, curr: Path, prevOption: Option[Path]): Unit = {
    val state = new Properties(p)
    val stateFile = curr.resolve(p.apply(DataStateFilename))
    val stateReader = Files.newBufferedReader(stateFile)
    state.load(stateReader)
    stateReader.close()

    try {
      runCompute(state, root, curr, "phase.minimal.min")
      runMinimalMinCompare(state, curr, prevOption)
      runConsolidation(state, curr, prevOption, "phase.minimal.min")
      for (phase <- Seq("phase.minimal.more-d", "phase.minimal.more-n")) {
        runCompute(state, root, curr, phase)
        runConsolidation(state, curr, prevOption, phase)
      }
    } finally {
      val stateWriter = Files.newBufferedWriter(stateFile)
      state.store(stateWriter, null)
      stateWriter.close()
    }
  }

  private[this] def execute(p: Properties, dir: Path): Unit = {
    val dumps = dir.resolve(p.apply(DataRoot))
    val children = Files.list(dumps).collect(Collectors.toList).asScala.sorted
    for (i <- children.indices) {
      executeOne(p, dir, children(i), children.lift.apply(i - 1))
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      usage()
    } else {
      val file = Paths.get(args(0))
      if (Files.exists(file)) {
        sys.error(s"Error: File '${args(0)} does not exist'")
        usage()
      }
      if (args.contains("--single-step")) {
        terminateOnPhaseCompletion = true
      }
      val reader = Files.newBufferedReader(file)
      val config = new Properties()
      config.load(reader)
      reader.close()
      try {
        execute(config, file.getParent)
      } catch {
        case e: StageTermination => println(e.getMessage)
      }
    }
  }
}
