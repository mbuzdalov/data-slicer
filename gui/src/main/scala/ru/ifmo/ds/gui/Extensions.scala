package ru.ifmo.ds.gui

import java.awt.{BorderLayout, Color, Paint}

import javax.swing._

import org.jfree.chart.labels.XYToolTipGenerator
import org.jfree.chart.plot.{PlotOrientation, XYPlot}
import org.jfree.chart.renderer.xy.{DeviationRenderer, XYLineAndShapeRenderer}
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.data.xy._

import ru.ifmo.ds.Database
import ru.ifmo.ds.gui.util._
import ru.ifmo.ds.util.{Axis, OrderingForStringWithNumbers}

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
            val options = db.valuesUnderKey(key)
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

  private def makeUpPlot(plot: XYPlot): Unit = {
    plot.setBackgroundPaint(Color.WHITE)
    plot.setRangeGridlinePaint(Color.BLACK)
    plot.setDomainGridlinePaint(Color.BLACK)
  }

  def makeXY(db: Database, categoryKeys: Seq[String], xAxis: Axis, yAxis: Axis, seriesKey: String): JComponent = {
    type LeafDescription = Seq[YIntervalSeries]

    def createJFreeChart(data: LeafDescription): JComponent = {
      assert(SwingUtilities.isEventDispatchThread)
      val jFreeData = new YIntervalSeriesCollection
      for (series <- data) {
        jFreeData.addSeries(series)
      }

      val plot = new XYPlot(jFreeData, xAxis.toJFreeChartAxis, yAxis.toJFreeChartAxis, CustomDeviationRenderer)
      plot.setOrientation(PlotOrientation.VERTICAL)
      makeUpPlot(plot)

      new ChartPanel(new JFreeChart(plot))
    }

    def composeSeries(db: Database): LeafDescription = {
      assert(!SwingUtilities.isEventDispatchThread)
      val contents = db.groupMap2D(_.get(seriesKey), _.get(xAxis.key).map(_.toDouble), _.get(yAxis.key).map(_.toDouble))
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

      val plot = new XYPlot(jFreeData, xAxis.toJFreeChartAxis, yAxis.toJFreeChartAxis, new XYLineAndShapeRenderer(false, true))
      plot.setOrientation(PlotOrientation.VERTICAL)
      makeUpPlot(plot)

      new ChartPanel(new JFreeChart(plot))
    }

    def tupledOption[A, X, Y](fun1: A => Option[X], fun2: A => Option[Y])(a: A): Option[(X, Y)] = {
      val vx = fun1(a)
      val vy = fun2(a)
      if (vx.nonEmpty && vy.nonEmpty) Some(vx.get -> vy.get) else None
    }

    def composeSeries(db: Database): LeafDescription = {
      assert(!SwingUtilities.isEventDispatchThread)
      val contents = db.groupMap2D(
        tupledOption(_.get(xAxis.key).map(_.toDouble), _.get(yAxis.key).map(_.toDouble)),
        _.get(seriesKey), _.get(compareByKey).map(_.toDouble))

      def findSmallestByMedian(arg: Map[String, Seq[Double]]): String = {
        val medians = arg.mapValues(b => b.sorted.apply(b.size / 2))
        medians.minBy(_._2)._1
      }

      val medians = contents.mapValues(findSmallestByMedian)
      val allValues = medians.values.toIndexedSeq.distinct.sorted(OrderingForStringWithNumbers.SpecialDotTreatment)
      val mediansSeq = medians.toIndexedSeq

      allValues map { seriesKey =>
        val myPairs = mediansSeq.filter(_._2 == seriesKey).map(_._1)
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
}
