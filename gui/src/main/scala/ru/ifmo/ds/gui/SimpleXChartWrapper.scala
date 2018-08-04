package ru.ifmo.ds.gui

import org.knowm.xchart.{XChartPanel, XYChart}
import ru.ifmo.ds.Database

import scala.collection.mutable

class SimpleXChartWrapper(
  width: Int, height: Int,
  xName: String, xKey: String,
  yName: String, yKey: String
) {
  private val chart = new XYChart(width, height)
  chart.setXAxisTitle(xName)
  chart.setYAxisTitle(yName)
  private val style = chart.getStyler
  style.setXAxisLogarithmic(true)
  style.setYAxisLogarithmic(true)

  val gui = new XChartPanel(chart)

  def addDatabase(db: Database, graphKey: String): Unit = {
    val contents = new mutable.HashMap[String, mutable.HashMap[Double, mutable.ArrayBuffer[Double]]]()
    db foreach { e =>
      if (e.contains(xKey) && e.contains(yKey) && e.contains(graphKey)) {
        val mapForGraphKey = contents.getOrElseUpdate(e(graphKey), new mutable.HashMap())
        val mapForXKey = mapForGraphKey.getOrElseUpdate(e(xKey).toDouble, new mutable.ArrayBuffer())
        mapForXKey += e(yKey).toDouble
      }
    }

    for ((plot, map) <- contents.toIndexedSeq.sortBy(_._1)) {
      val xs, ys, errs = Array.newBuilder[Double]
      for ((x, y) <- map.toIndexedSeq.sortBy(_._1.toInt)) {
        xs += x
        val yMin = y.min
        val yMax = y.max
        ys += (yMin + yMax) / 2
        errs += (yMax - yMin) / 2
      }
      chart.addSeries(plot, xs.result(), ys.result(), errs.result())
    }
  }
}
