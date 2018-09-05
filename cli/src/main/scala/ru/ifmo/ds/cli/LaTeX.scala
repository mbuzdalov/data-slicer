package ru.ifmo.ds.cli

import java.io.{File, PrintStream}

import ru.ifmo.ds.io.Json
import ru.ifmo.ds.util.{Axis, OrderingForStringWithNumbers}
import ru.ifmo.ds.{CLI, Database}

object LaTeX extends CLI.Module {
  override def name: String = "latex"

  override def apply(args: Array[String]): Unit = {
    val filesOpt = new CommandLineOption("--files", 1)
    val xAxisOpt = new CommandLineOption("--x-axis", 2, 2)
    val yAxisOpt = new CommandLineOption("--y-axis", 2, 2)
    val seriesKeyOpt = new CommandLineOption("--series-key", 1, 1)
    val categoryKeysOpt = new CommandLineOption("--category-keys")

    CommandLineOption.submit(Seq(filesOpt, xAxisOpt, yAxisOpt, seriesKeyOpt, categoryKeysOpt), args :_*)

    def parseAxis(args: IndexedSeq[String]): Axis = Axis("??", args(0), args(1) == "log")

    val xAxis = parseAxis(xAxisOpt.result())
    val yAxis = parseAxis(yAxisOpt.result())
    val seriesKey = seriesKeyOpt.result().head
    val categoryKeys = categoryKeysOpt.result()
    val files = filesOpt.result()

    def run(db: Database, categoryKeys: IndexedSeq[String], prefix: String): Unit = {
      def processCats(categoryKeys: IndexedSeq[String]): Unit = {
        if (!db.hasEntries || categoryKeys.isEmpty) {
          val plotData = db.groupMap2D(_.get(seriesKey), _.get(xAxis.key).map(_.toDouble), _.get(yAxis.key).map(_.toDouble))
          println("%% " + prefix)
          println("\\begin{tikzpicture}[scale=\\picturescale]")
          println(s"\\begin{axis}[xtick=data, ${
            if (xAxis.isLogarithmic) "xmode=log," else ""
          } ${
            if (yAxis.isLogarithmic) "ymode=log," else ""
          } width=\\picturewidth, height=\\pictureheight, legend pos=outer north east, cycle list name=\\picturecycle]")

          for ((series, seriesData) <- plotData) {
            println("\\addplot table {")
            println("    x y")
            for ((x, ys) <- seriesData.toIndexedSeq.sortBy(_._1)) {
              val avg = ys.sum / ys.size
              println(s"  $x $avg")
            }
            println("};")
            println(s"\\addlegendentry{$series};")
          }

          println("\\end{axis}")
          println("\\end{tikzpicture}")
        } else {
          val key = categoryKeys.head
          val options = db.valuesUnderKey(key)
          assert(options.nonEmpty)
          if (options.size == 1) {
            processCats(categoryKeys.tail)
          } else {
            val grouped = db.entries.groupBy(_.get(key))
            val withProperName = grouped.map(p => (p._1.getOrElse("<none>"), Database(p._2: _*)))
            val sorted = withProperName.toIndexedSeq.sortBy(_._1)(OrderingForStringWithNumbers.SpecialDotTreatment)
            sorted.foreach(p => run(p._2, categoryKeys.tail, prefix + "." + p._1))
          }
        }
      }
      processCats(categoryKeys)
    }

    run(Database.merge(files.map(f => Json.fromFile(new File(f))) :_*), categoryKeys, "")
  }

  override def printUsage(out: PrintStream): Unit = {
    out.println(s"  $name --files <database-files...> --x-axis <key> log|lin --y-axis <key> log|lin --series-key <key> --category-keys <keys...>.")
    out.println("       Creates a LaTeX file with plots read from the given databases.")
    out.println("       Each plot will have the given keys for the X axis and the Y axis (logarithmic or linear),")
    out.println("       each line in each plot corresponds to the given series key, and the database will be split")
    out.println("       into slices (and hence into multiple plots) according to the given category keys.")
  }
}
