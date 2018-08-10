package ru.ifmo.ds.gui

import java.awt.{BorderLayout, Color, Paint}

import javax.swing._

import scala.collection.mutable

import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.chart.axis.{LogarithmicAxis, NumberAxis}
import org.jfree.chart.labels.XYToolTipGenerator
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.renderer.xy.DeviationRenderer
import org.jfree.data.xy.{IntervalXYDataset, XYDataset, YIntervalSeries, YIntervalSeriesCollection}

import ru.ifmo.ds.Database
import ru.ifmo.ds.util.OrderingForStringWithNumbers

object Extensions {
  def makePresentationTree[T](db: Database, categoryKeys: Seq[String], makeLeaf: Database => T, makeView: T => JComponent): JComponent = {
    type WorkerResult = Either[(Seq[String], Seq[(String, Database)]), T]
    val result = new JPanel(new BorderLayout())
    val worker = new SwingWorker[WorkerResult, Unit] {
      override def doInBackground(): WorkerResult = {
        def processCats(categoryKeys: Seq[String]): WorkerResult = {
          if (!db.hasEntries || categoryKeys.isEmpty) {
            Right(makeLeaf(db))
          } else {
            val key = categoryKeys.head
            val options = db.entries.map(_.get(key)).distinct
            assert(options.nonEmpty)
            if (options.size == 1) {
              processCats(categoryKeys.tail)
            } else {
              val grouped = db.entries.groupBy(_.get(key))
              val withProperName = grouped.map(p => (p._1.getOrElse("<none>"), Database(p._2 :_*)))
              val sorted = withProperName.toIndexedSeq.sortBy(_._1)(OrderingForStringWithNumbers.SpecialDotTreatment)
              Left(categoryKeys.tail, sorted)
            }
          }
        }
        processCats(categoryKeys)
      }

      override def done(): Unit = {
        get() match {
          case Left((cats, children)) =>
            val tabbedPane = new JTabbedPane()
            result.add(tabbedPane, BorderLayout.CENTER)
            for ((title, db) <- children) {
              tabbedPane.add(title, makePresentationTree(db, cats, makeLeaf, makeView))
            }
          case Right(plotInput) =>
            result.add(makeView(plotInput), BorderLayout.CENTER)
        }
      }
    }
    worker.execute()
    result
  }

  def makeXY(db: Database, categoryKeys: Seq[String], xAxis: Axis, yAxis: Axis, seriesKey: String): JComponent = {
    type LeafDescription = Seq[YIntervalSeries]

    def toJFreeChartAxis(axis: Axis): NumberAxis = if (axis.isLogarithmic) {
      new LogarithmicAxis(axis.name)
    } else {
      new NumberAxis(axis.name)
    }

    def createJFreeChart(data: LeafDescription): JComponent = {
      assert(SwingUtilities.isEventDispatchThread)
      val jFreeData = new YIntervalSeriesCollection
      for (series <- data) {
        jFreeData.addSeries(series)
      }

      val jFreeChart = ChartFactory.createXYLineChart("", xAxis.name, yAxis.name, jFreeData,
        PlotOrientation.VERTICAL, true, true, false)
      val jFreePanel = new ChartPanel(jFreeChart)

      val plot = jFreeChart.getXYPlot
      val renderer = new DeviationRenderer(true, true) {
        override def lookupSeriesFillPaint(series: Int): Paint = {
          super.lookupSeriesPaint(series)
        }
      }
      renderer.setAlpha(0.5f)
      renderer.setDefaultToolTipGenerator(ExtendedTooltipGenerator)
      plot.setRenderer(renderer)
      plot.setBackgroundPaint(Color.WHITE)
      plot.setRangeGridlinePaint(Color.BLACK)
      plot.setDomainGridlinePaint(Color.BLACK)
      plot.setDomainAxis(toJFreeChartAxis(xAxis))
      plot.setRangeAxis(toJFreeChartAxis(yAxis))
      jFreePanel
    }

    def composeSeries(db: Database): LeafDescription = {
      assert(!SwingUtilities.isEventDispatchThread)
      val contents = new mutable.HashMap[String, mutable.HashMap[Double, mutable.ArrayBuffer[Double]]]()
      db foreach { e =>
        if (e.contains(xAxis.key) && e.contains(yAxis.key) && e.contains(seriesKey)) {
          val mapForGraphKey = contents.getOrElseUpdate(e(seriesKey), new mutable.HashMap())
          val mapForXKey = mapForGraphKey.getOrElseUpdate(e(xAxis.key).toDouble, new mutable.ArrayBuffer())
          mapForXKey += e(yAxis.key).toDouble
        }
      }

      val sortedContents = contents.toIndexedSeq.sortBy(_._1)(OrderingForStringWithNumbers.SpecialDotTreatment)
      sortedContents.map { case (plot, map) =>
        val series = new YIntervalSeries(plot)
        for ((x, y) <- map.toIndexedSeq.sortBy(_._1.toInt)) {
          val ySorted = y.toIndexedSeq.sorted
          series.add(x, ySorted(ySorted.size / 2), ySorted.head, ySorted.last)
        }
        series
      }
    }

    makePresentationTree(db, categoryKeys, composeSeries, createJFreeChart)
  }

  private object ExtendedTooltipGenerator extends XYToolTipGenerator {
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
