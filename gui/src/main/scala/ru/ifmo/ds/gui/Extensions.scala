package ru.ifmo.ds.gui

import java.awt.{BorderLayout, Color, Paint}

import javax.swing._

import scala.collection.mutable

import org.jfree.chart.labels.XYToolTipGenerator
import org.jfree.chart.plot.{PlotOrientation, XYPlot}
import org.jfree.chart.renderer.xy.DeviationRenderer
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy._

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

  private def makeUpPlot(plot: XYPlot, xAxis: Axis, yAxis: Axis): Unit = {
    plot.setBackgroundPaint(Color.WHITE)
    plot.setRangeGridlinePaint(Color.BLACK)
    plot.setDomainGridlinePaint(Color.BLACK)
    plot.setDomainAxis(xAxis.toJFreeChartAxis)
    plot.setRangeAxis(yAxis.toJFreeChartAxis)
  }

  def makeXY(db: Database, categoryKeys: Seq[String], xAxis: Axis, yAxis: Axis, seriesKey: String): JComponent = {
    type LeafDescription = Seq[YIntervalSeries]

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
      makeUpPlot(plot, xAxis, yAxis)
      plot.setRenderer(CustomDeviationRenderer)
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

  def makeWhoIsBest(db: Database, categoryKeys: Seq[String], xAxis: Axis, yAxis: Axis, seriesKey: String, compareByKey: String): JComponent = {
    type LeafDescription = Seq[XYSeries]

    def createJFreeChart(data: LeafDescription): JComponent = {
      assert(SwingUtilities.isEventDispatchThread)
      val jFreeData = new XYSeriesCollection()
      for (series <- data) {
        jFreeData.addSeries(series)
      }

      val jFreeChart = ChartFactory.createScatterPlot("", xAxis.name, yAxis.name, jFreeData,
        PlotOrientation.VERTICAL, true, true, false)
      val jFreePanel = new ChartPanel(jFreeChart)
      val plot = jFreeChart.getXYPlot
      makeUpPlot(plot, xAxis, yAxis)
      jFreePanel
    }

    def composeSeries(db: Database): LeafDescription = {
      assert(!SwingUtilities.isEventDispatchThread)
      val contents = new mutable.HashMap[(Double, Double), mutable.HashMap[String, mutable.ArrayBuffer[Double]]]()
      db foreach { e =>
        if (e.contains(xAxis.key) && e.contains(yAxis.key) && e.contains(seriesKey) && e.contains(compareByKey)) {
          val mapForPointKey = contents.getOrElseUpdate(e(xAxis.key).toDouble -> e(yAxis.key).toDouble, new mutable.HashMap())
          val mapForSeriesKey = mapForPointKey.getOrElseUpdate(e(seriesKey), new mutable.ArrayBuffer())
          mapForSeriesKey += e(compareByKey).toDouble
        }
      }

      def findSmallestByMedian(arg: mutable.HashMap[String, mutable.ArrayBuffer[Double]]): String = {
        val medians = arg.mapValues(b => b.sorted.apply(b.size / 2))
        medians.minBy(_._2)._1
      }

      val medians = contents.mapValues(findSmallestByMedian)
      val allValues = medians.values.toIndexedSeq.distinct.sorted(OrderingForStringWithNumbers.SpecialDotTreatment)

      allValues map { seriesKey =>
        val myPairs = medians.filter(_._2 == seriesKey).map(_._1)
        val series = new XYSeries(seriesKey)
        myPairs.foreach(p => series.add(p._1, p._2))
        series
      }
    }

    makePresentationTree(db, categoryKeys, composeSeries, createJFreeChart)
  }

  private object CustomDeviationRenderer extends DeviationRenderer {
    setAlpha(0.5f)
    setDefaultToolTipGenerator(ExtendedTooltipGenerator)
    override def lookupSeriesFillPaint(series: Int): Paint = {
      super.lookupSeriesPaint(series)
    }
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
        case d => generateToolTips(d, series, item)
      }
      val sortedRows = allRows.sortBy(-_._1).map(_._2) // tooltip rows top-down
      sortedRows.mkString("<html>", "<br/>", "</html>")
    }
  }
}
