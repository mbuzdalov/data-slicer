package ru.ifmo.ds.gui

import java.awt._

import javax.swing._

import org.jfree.chart.plot.{PlotOrientation, XYPlot}
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.data.xy._

import ru.ifmo.ds.Database
import ru.ifmo.ds.gui.util.JFreeUtils._
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
      initializePlotColors(plot)

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
}
