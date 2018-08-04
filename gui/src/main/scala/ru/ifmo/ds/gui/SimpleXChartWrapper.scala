package ru.ifmo.ds.gui

import org.knowm.xchart.style.Styler.ToolTipType
import org.knowm.xchart.{XChartPanel, XYChart}
import ru.ifmo.ds.Database

import scala.collection.mutable

class SimpleXChartWrapper(width: Int, height: Int, xAxis: Axis, yAxis: Axis) {
  private val chart = new XYChart(width, height)
  chart.setXAxisTitle(xAxis.name)
  chart.setYAxisTitle(yAxis.name)
  private val style = chart.getStyler
  style.setXAxisLogarithmic(xAxis.isLogarithmic)
  style.setYAxisLogarithmic(yAxis.isLogarithmic)
  style.setToolTipType(ToolTipType.xAndYLabels)
  style.setToolTipsEnabled(true)

  val gui = new XChartPanel(chart)

  def addDatabase(db: Database, graphKey: String): Unit = {
    val contents = new mutable.HashMap[String, mutable.HashMap[Double, mutable.ArrayBuffer[Double]]]()
    db foreach { e =>
      if (e.contains(xAxis.key) && e.contains(yAxis.key) && e.contains(graphKey)) {
        val mapForGraphKey = contents.getOrElseUpdate(e(graphKey), new mutable.HashMap())
        val mapForXKey = mapForGraphKey.getOrElseUpdate(e(xAxis.key).toDouble, new mutable.ArrayBuffer())
        mapForXKey += e(yAxis.key).toDouble
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
