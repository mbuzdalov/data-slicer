package ru.ifmo.ds.gui

import java.awt.{Color, Paint}

import javax.swing.JSplitPane
import org.jfree.chart.axis.{LogarithmicAxis, NumberAxis}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.DeviationRenderer
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy.{YIntervalSeries, YIntervalSeriesCollection}
import org.knowm.xchart.style.Styler.ToolTipType
import org.knowm.xchart.{XChartPanel, XYChart}
import ru.ifmo.ds.Database
import ru.ifmo.ds.util.OrderingForStringWithNumbers

import scala.collection.mutable

class SimpleChartWrapper(width: Int, height: Int, xAxis: Axis, yAxis: Axis) {
  private val xChart = new XYChart(width, height)
  xChart.setXAxisTitle(xAxis.name)
  xChart.setYAxisTitle(yAxis.name)
  private val style = xChart.getStyler
  style.setXAxisLogarithmic(xAxis.isLogarithmic)
  style.setYAxisLogarithmic(yAxis.isLogarithmic)
  style.setToolTipType(ToolTipType.xAndYLabels)
  style.setToolTipsEnabled(true)

  private val xPanel = new XChartPanel(xChart)

  private val jFreeData = new YIntervalSeriesCollection
  private val jFreeChart = ChartFactory.createXYLineChart("", xAxis.name, yAxis.name, jFreeData,
    PlotOrientation.VERTICAL, true, true, false)
  private val jFreePanel = new ChartPanel(jFreeChart)

  private def toJFreeChartAxis(axis: Axis): NumberAxis = if (axis.isLogarithmic) {
    new LogarithmicAxis(axis.name)
  } else {
    new NumberAxis(axis.name)
  }

  locally {
    val plot = jFreeChart.getXYPlot
    val renderer = new DeviationRenderer(true, true) {
      override def lookupSeriesFillPaint(series: Int): Paint = {
        super.lookupSeriesPaint(series)
      }
    }
    renderer.setAlpha(0.5f)
    plot.setRenderer(renderer)
    plot.setBackgroundPaint(Color.WHITE)
    plot.setRangeGridlinePaint(Color.BLACK)
    plot.setDomainGridlinePaint(Color.BLACK)
    plot.setDomainAxis(toJFreeChartAxis(xAxis))
    plot.setRangeAxis(toJFreeChartAxis(yAxis))
  }

  val gui = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, xPanel, jFreePanel)
  gui.setResizeWeight(0.5)

  def addDatabase(db: Database, graphKey: String): Unit = {
    val contents = new mutable.HashMap[String, mutable.HashMap[Double, mutable.ArrayBuffer[Double]]]()
    db foreach { e =>
      if (e.contains(xAxis.key) && e.contains(yAxis.key) && e.contains(graphKey)) {
        val mapForGraphKey = contents.getOrElseUpdate(e(graphKey), new mutable.HashMap())
        val mapForXKey = mapForGraphKey.getOrElseUpdate(e(xAxis.key).toDouble, new mutable.ArrayBuffer())
        mapForXKey += e(yAxis.key).toDouble
      }
    }

    for ((plot, map) <- contents.toIndexedSeq.sortBy(_._1)(OrderingForStringWithNumbers.SpecialDotTreatment)) {
      val xs, ys, errs = Array.newBuilder[Double]
      val series = new YIntervalSeries(plot)
      for ((x, y) <- map.toIndexedSeq.sortBy(_._1.toInt)) {
        xs += x
        val yMin = y.min
        val yMax = y.max
        ys += (yMin + yMax) / 2
        errs += (yMax - yMin) / 2

        val ySorted = y.toIndexedSeq.sorted
        series.add(x, ySorted(ySorted.size / 2), ySorted.head, ySorted.last)
      }
      jFreeData.addSeries(series)
      xChart.addSeries(plot, xs.result(), ys.result(), errs.result())
    }
  }
}
