package ru.ifmo.ds.gui

import java.awt.{Color, Paint}

import javax.swing.JPanel
import org.jfree.chart.axis.{LogarithmicAxis, NumberAxis}
import org.jfree.chart.labels.XYToolTipGenerator
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.DeviationRenderer
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy.{IntervalXYDataset, XYDataset, YIntervalSeries, YIntervalSeriesCollection}
import ru.ifmo.ds.Database
import ru.ifmo.ds.util.OrderingForStringWithNumbers

import scala.collection.mutable

class SimpleChartWrapper(width: Int, height: Int, xAxis: Axis, yAxis: Axis) {
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
    renderer.setDefaultToolTipGenerator(SimpleChartWrapper.ExtendedTooltipGenerator)
    plot.setRenderer(renderer)
    plot.setBackgroundPaint(Color.WHITE)
    plot.setRangeGridlinePaint(Color.BLACK)
    plot.setDomainGridlinePaint(Color.BLACK)
    plot.setDomainAxis(toJFreeChartAxis(xAxis))
    plot.setRangeAxis(toJFreeChartAxis(yAxis))
  }

  val gui: JPanel = jFreePanel

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
      val series = new YIntervalSeries(plot)
      for ((x, y) <- map.toIndexedSeq.sortBy(_._1.toInt)) {
        val ySorted = y.toIndexedSeq.sorted
        series.add(x, ySorted(ySorted.size / 2), ySorted.head, ySorted.last)
      }
      jFreeData.addSeries(series)
    }
  }
}

object SimpleChartWrapper {
  object ExtendedTooltipGenerator extends XYToolTipGenerator {
    private def generateToolTips(dataset: XYDataset, series: Int, item: Int): Array[(Double, String)] = {
      Array.tabulate(dataset.getSeriesCount) { s =>
        val x = dataset.getX(s, item).intValue()
        val y = dataset.getYValue(s, item)
        val prefix = if (s == series) "<b>" else ""
        val suffix = if (s == series) "</b>" else ""
        (y, f"$prefix${dataset.getSeriesKey(s)}: $x => $y%.3e$suffix")
      }
    }

    private def generateIntervalToolTips(dataset: IntervalXYDataset, series: Int, item: Int): Array[(Double, String)] = {
      Array.tabulate(dataset.getSeriesCount) { s =>
        val x = dataset.getX(s, item).intValue()
        val y = dataset.getYValue(s, item)
        val yMin = dataset.getStartYValue(s, item)
        val yMax = dataset.getEndYValue(s, item)
        val prefix = if (s == series) "<b>" else ""
        val suffix = if (s == series) "</b>" else ""
        (y, f"$prefix${dataset.getSeriesKey(s)}: $x => [$yMin%.3e; $y%.3e, $yMax%.3e]$suffix")
      }
    }

    override def generateToolTip(dataset: XYDataset, series: Int, item: Int): String = {
      val allRows = dataset match {
        case d: IntervalXYDataset => generateIntervalToolTips(d, series, item)
        case _ => generateToolTips(dataset, series, item)
      }
      val sortedRows = allRows.sortBy(-_._1).map(_._2) // tooltip rows top-down
      sortedRows.mkString("<html>", "<br/>", "</html>")
    }
  }
}
