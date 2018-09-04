package ru.ifmo.ds.cli

import java.io.PrintStream

import ru.ifmo.ds.util.{Axis, OrderingForStringWithNumbers}
import ru.ifmo.ds.{CLI, Database}

object LaTeX extends CLI.Module {
  override def name: String = "latex"

  override def apply(args: Array[String]): Unit = {
    val xAxis: Axis = ???
    val yAxis: Axis = ???
    val seriesKey: String = ???

    def run(db: Database, categoryKeys: IndexedSeq[String], prefix: String): Unit = {
      def processCats(categoryKeys: IndexedSeq[String]): Unit = {
        if (!db.hasEntries || categoryKeys.isEmpty) {
          val plotData = db.groupMap2D(_.get(seriesKey), _.get(xAxis.key).map(_.toDouble), _.get(yAxis.key).map(_.toDouble))
          println("\\begin{tikzpicture}[scale=\\picturescale]")
          println(s"\\begin{axis}[xtick=data, ${
            if (xAxis.isLogarithmic) "xmode=log," else ""
          }, ${
            if (yAxis.isLogarithmic) "ymode=log," else ""
          } width=\\picturewidth, height=\\pictureheight, legend pos=outer north east, cycle name=\\picturecycle]")

          for ((series, seriesData) <- plotData) {
            println("\\addplot plot[error bars/.cd, y dir=both, y explicit] table[y error plus=y-max, y error minus=y-min] {")
            println("    x y y-min y-max")
            for ((x, ys) <- seriesData) {
              val avg = ys.sum / ys.size
              println(s"  $x ${ys.min} $avg ${ys.max}")
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
  }

  override def printUsage(out: PrintStream): Unit = {

  }
}
