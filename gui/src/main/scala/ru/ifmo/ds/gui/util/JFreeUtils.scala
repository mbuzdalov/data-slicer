package ru.ifmo.ds.gui.util

import java.awt.{Color, Font, Paint}

import scala.Ordering.Double.IeeeOrdering

import org.jfree.chart.axis.{LogarithmicAxis, NumberAxis, ValueAxis}
import org.jfree.chart.{ChartMouseEvent, ChartMouseListener, ChartPanel}
import org.jfree.chart.entity.LegendItemEntity
import org.jfree.chart.labels.XYToolTipGenerator
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.DeviationRenderer
import org.jfree.data.xy.{IntervalXYDataset, XYDataset}

import ru.ifmo.ds.util.Axis

object JFreeUtils {
  implicit class AxisOps(val axis: Axis) extends AnyVal {
    def toJFreeChartAxis: ValueAxis = if (axis.isLogarithmic) {
      new LogarithmicAxis(axis.name)
    } else {
      new NumberAxis(axis.name)
    }
  }

  class CustomDeviationRenderer extends DeviationRenderer {
    setAlpha(0.5f)
    setDefaultToolTipGenerator(JFreeUtils.extendedTooltipGenerator)
    override def lookupSeriesFillPaint(series: Int): Paint = {
      super.lookupSeriesPaint(series)
    }
  }

  def initializePlotColors(plot: XYPlot): Unit = {
    plot.setBackgroundPaint(Color.WHITE)
    plot.setRangeGridlinePaint(Color.BLACK)
    plot.setDomainGridlinePaint(Color.BLACK)
  }

  def augmentWithTogglingPlotsByClickingOnLegend(panel: ChartPanel): Unit = {
    val plot = panel.getChart.getXYPlot
    // Fix the legend, so that making a plot invisible does not hide it from the legend
    val legend = plot.getLegendItems
    plot.setFixedLegendItems(legend)
    panel.addChartMouseListener(toggleListener)
  }

  val extendedTooltipGenerator: XYToolTipGenerator = new XYToolTipGenerator {
    private def generateToolTips(dataset: XYDataset, series: Int, item: Int): Array[(Double, String)] = {
      (0 until dataset.getSeriesCount).flatMap { s =>
        if (dataset.getItemCount(s) > item) {
          val x = dataset.getX(s, item).intValue()
          val y = dataset.getYValue(s, item)
          val prefix = if (s == series) "<b>" else ""
          val suffix = if (s == series) "</b>" else ""
          Some(y -> f"$prefix${dataset.getSeriesKey(s)}: $x => $y%.3e$suffix")
        } else None
      }.toArray
    }

    private def generateIntervalToolTips(dataset: IntervalXYDataset, series: Int, item: Int): Array[(Double, String)] = {
      (0 until dataset.getSeriesCount).flatMap { s =>
        if (dataset.getItemCount(s) > item) {
          val x = dataset.getX(s, item).intValue()
          val y = dataset.getYValue(s, item)
          val yMin = dataset.getStartYValue(s, item)
          val yMax = dataset.getEndYValue(s, item)
          val prefix = if (s == series) "<b>" else ""
          val suffix = if (s == series) "</b>" else ""
          Some(y -> f"$prefix${dataset.getSeriesKey(s)}: $x => [$yMin%.3e; $y%.3e, $yMax%.3e]$suffix")
        } else None
      }.toArray
    }

    override def generateToolTip(dataset: XYDataset, series: Int, item: Int): String = {
      val allRows = dataset match {
        case d: IntervalXYDataset => generateIntervalToolTips(d, series, item)
        case d => generateToolTips(d, series, item)
      }
      val sortedRows = allRows.sortBy(-_._1).map(_._2) // tooltip rows top-down
      sortedRows.mkString("<html>", "<br/>", "</html>")
    }
  }

  private val toggleListener = new ChartMouseListener {
    override def chartMouseClicked(event: ChartMouseEvent): Unit = {
      event.getEntity match {
        case l: LegendItemEntity =>
          val ds = l.getDataset.asInstanceOf[XYDataset]
          val index = ds.indexOf(l.getSeriesKey)
          val plot = event.getChart.getXYPlot
          val renderer = plot.getRenderer
          val oldVisible = renderer.getSeriesVisible(index)
          renderer.setSeriesVisible(index, if (oldVisible == null) false else !oldVisible, true)
          val legendItems = plot.getFixedLegendItems
          for (i <- 0 until legendItems.getItemCount) {
            val legendItem = legendItems.get(i)
            if (legendItem.getDataset == ds && legendItem.getSeriesIndex == index) {
              val oldFont0 = legendItem.getLabelFont
              val oldFont = if (oldFont0 == null) new Font(Font.SANS_SERIF, Font.PLAIN, 12) else oldFont0
              if (oldFont.isPlain) {
                legendItem.setLabelFont(oldFont.deriveFont(Font.ITALIC))
                legendItem.setLabelPaint(Color.GRAY)
              } else {
                legendItem.setLabelFont(oldFont.deriveFont(Font.PLAIN))
                legendItem.setLabelPaint(Color.BLACK)
              }
            }
          }
        case _ =>
      }
    }

    override def chartMouseMoved(event: ChartMouseEvent): Unit = {}
  }
}
