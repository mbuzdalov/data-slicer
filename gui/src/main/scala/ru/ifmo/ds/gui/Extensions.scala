package ru.ifmo.ds.gui

import java.awt._
import java.util.{Arrays, Comparator, Locale}

import javax.swing._
import javax.swing.event.TableModelListener
import javax.swing.table._

import scala.collection.{mutable => mu}

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

  private class TableDoubleValueDisplay(val median: Double,
                                        val min: Double,
                                        val max: Double,
                                        alwaysSci: Boolean) extends Comparable[TableDoubleValueDisplay] {
    private[this] def makeSci(value: Double): String = {
      val str = "%.03e".formatLocal(Locale.US, value)
      val eIdx = str.indexOf('e')
      val rv = if (eIdx < 0) str else {
        val prefix = str.substring(0, eIdx)
        val suffix = str.substring(eIdx + 1).toInt
        prefix + "\u00b710<sup>" + suffix + "</sup>"
      }
      rv.replace("-", "&minus;")
    }

    private[this] def constructValue(value: Double): String = {
      if (alwaysSci) {
        makeSci(value)
      } else {
        val defVal = value.toString
        if (defVal.length > 7) makeSci(value) else defVal.replace("-", "&minus;")
      }
    }

    def minText: String = constructValue(min)
    def maxText: String = constructValue(max)
    def medText: String = constructValue(median)

    override val toString: String = if (median.isNaN) "---" else "<html>" + constructValue(median) + "</html>"

    override def compareTo(o: TableDoubleValueDisplay): Int = {
      if (median.isNaN) {
        if (o.median.isNaN) 0 else 1
      } else if (o.median.isNaN) {
        -1
      } else {
        java.lang.Double.compare(median, o.median)
      }
    }
  }

  private object TableDoubleValueDisplay {
    final val NaN = new TableDoubleValueDisplay(Double.NaN, Double.NaN, Double.NaN, true)
    final val CellRenderer = new TableCellRenderer {
      override def getTableCellRendererComponent(table: JTable, value: Any,
                                                 isSelected: Boolean, hasFocus: Boolean,
                                                 row: Int, column: Int): Component = {
        value match {
          case v: TableDoubleValueDisplay if !v.min.isNaN && !v.max.isNaN =>
            val rv = new JLabel(v.toString)
            rv.setToolTipText(s"<html>median = ${v.medText}<br/>min = ${v.minText}<br/>max = ${v.maxText}</html>")
            rv
          case _ => new JLabel(value.toString)
        }
      }
    }
  }

  private def makeTableRow(data: YIntervalSeries, xValues: Array[Double]): Array[AnyRef] = {
    val rv = Array.tabulate[AnyRef](xValues.length + 1)(i => if (i == 0) data.getKey.asInstanceOf[AnyRef] else TableDoubleValueDisplay.NaN)
    for (itemIndex <- 0 until data.getItemCount) {
      val x = data.getX(itemIndex).doubleValue()
      //noinspection ReferenceMustBePrefixed
      val ix = Arrays.binarySearch(xValues, x)
      assert(ix >= 0)
      rv(ix + 1) = new TableDoubleValueDisplay(data.getYValue(itemIndex), data.getYLowValue(itemIndex), data.getYHighValue(itemIndex), true)
    }
    rv
  }

  private def prepareTable(data: Seq[YIntervalSeries]): (TableModel, TableRowSorter[TableModel]) = {
    val xSet = new mu.TreeSet[Double]
    for (series <- data) {
      for (itemIndex <- 0 until series.getItemCount) {
        xSet += series.getX(itemIndex).doubleValue()
      }
    }
    val xValues = xSet.toArray
    val tableContents = data.map(s => makeTableRow(s, xValues)).toArray
    val columnNames = "" +: xValues.map(v => new TableDoubleValueDisplay(v, Double.NaN, Double.NaN, false).toString)
    val tableModel = new TableModel {
      override def getRowCount: Int = tableContents.length
      override def getColumnCount: Int = columnNames.length
      override def getColumnName(columnIndex: Int): String = columnNames(columnIndex)
      override def getColumnClass(columnIndex: Int): Class[_] = if (columnIndex == 0) classOf[String] else classOf[TableDoubleValueDisplay]
      override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = false
      override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = tableContents(rowIndex)(columnIndex)
      override def setValueAt(aValue: Any, rowIndex: Int, columnIndex: Int): Unit = throw new UnsupportedOperationException("The table is unmodifiable")
      override def addTableModelListener(l: TableModelListener): Unit = {}
      override def removeTableModelListener(l: TableModelListener): Unit = {}
    }
    val tableSorter = new TableRowSorter[TableModel](tableModel)
    for (i <- 1 until columnNames.length) {
      tableSorter.setComparator(i, Comparator.naturalOrder[TableDoubleValueDisplay]())
    }
    (tableModel, tableSorter)
  }

  private def wrapInControllableSplitter(left: JComponent, leftButtonTooltip: String, leftButtonText: String,
                                         right: JComponent, rightButtonTooltip: String, rightButtonText: String): JComponent = {
    val split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, left, right)
    split.setDividerLocation(0.5)
    var lastHorizontalRelativeLocation = 0.5
    var lastVerticalRelativeLocation = 0.5

    def saveLocation(): Unit = {
      if (left.isVisible && right.isVisible) {
        if (split.getOrientation == JSplitPane.HORIZONTAL_SPLIT) {
          lastHorizontalRelativeLocation = split.getDividerLocation.toDouble / (split.getWidth - split.getDividerSize)
        } else {
          lastVerticalRelativeLocation = split.getDividerLocation.toDouble / (split.getHeight - split.getDividerSize)
        }
      }
    }

    val buttonRightOnly = new JButton(rightButtonText)
    buttonRightOnly.setToolTipText(rightButtonTooltip)
    buttonRightOnly.addActionListener(_ => {
      saveLocation()
      right.setVisible(true)
      left.setVisible(false)
      split.revalidate()
    })
    val buttonLeftOnly = new JButton(leftButtonText)
    buttonLeftOnly.setToolTipText(leftButtonTooltip)
    buttonLeftOnly.addActionListener(_ => {
      saveLocation()
      right.setVisible(false)
      left.setVisible(true)
      split.revalidate()
    })
    val buttonH = new JButton("|")
    buttonH.setToolTipText("Show both side-to-side")
    buttonH.addActionListener(_ => {
      saveLocation()
      right.setVisible(true)
      left.setVisible(true)
      split.setOrientation(JSplitPane.HORIZONTAL_SPLIT)
      split.setDividerLocation(lastHorizontalRelativeLocation)
      split.revalidate()
    })
    val buttonV = new JButton("<html>&minus;</html>")
    buttonV.setToolTipText("Show both atop one another")
    buttonV.addActionListener(_ => {
      saveLocation()
      right.setVisible(true)
      left.setVisible(true)
      split.setOrientation(JSplitPane.VERTICAL_SPLIT)
      split.setDividerLocation(lastVerticalRelativeLocation)
      split.revalidate()
    })

    val buttonPane = new JPanel()
    val buttonPaneLayout = new GroupLayout(buttonPane)
    buttonPaneLayout.setAutoCreateGaps(true)
    buttonPaneLayout.setAutoCreateContainerGaps(true)
    buttonPane.setLayout(buttonPaneLayout)

    buttonPaneLayout.setVerticalGroup(buttonPaneLayout
                                        .createSequentialGroup()
                                        .addGroup(buttonPaneLayout
                                                    .createParallelGroup()
                                                    .addComponent(buttonLeftOnly)
                                                    .addComponent(buttonRightOnly))
                                        .addGroup(buttonPaneLayout
                                                    .createParallelGroup()
                                                    .addComponent(buttonH)
                                                    .addComponent(buttonV)))
    buttonPaneLayout.setHorizontalGroup(buttonPaneLayout
                                          .createSequentialGroup()
                                          .addGroup(buttonPaneLayout
                                                      .createParallelGroup()
                                                      .addComponent(buttonLeftOnly)
                                                      .addComponent(buttonH))
                                          .addGroup(buttonPaneLayout
                                                      .createParallelGroup()
                                                      .addComponent(buttonRightOnly)
                                                      .addComponent(buttonV)))

    val rv = new JPanel(new BorderLayout())
    rv.add(split, BorderLayout.CENTER)
    rv.add(buttonPane, BorderLayout.LINE_START)
    rv
  }

  def makeXY(db: Database, categoryKeys: Seq[String], xAxis: Axis, yAxis: Axis, seriesKey: String): JComponent = {
    case class LeafDescription(plot: XYPlot,
                               tableModel: TableModel,
                               tableRowSorter: TableRowSorter[TableModel])

    def createJFreeChart(data: LeafDescription): JComponent = {
      assert(SwingUtilities.isEventDispatchThread)
      val chartPanel = new ChartPanel(new JFreeChart(data.plot))
      val table = new JTable(data.tableModel)
      table.setRowSorter(data.tableRowSorter)
      table.setDefaultRenderer(classOf[TableDoubleValueDisplay], TableDoubleValueDisplay.CellRenderer)
      val tableScroll = new JScrollPane(table)
      wrapInControllableSplitter(chartPanel, "Show the chart only", "C",
                                 tableScroll, "Show the table only", "T")
    }

    def composeSeries(db: Database): LeafDescription = {
      assert(!SwingUtilities.isEventDispatchThread)
      val contents = db.groupMap2D(_.get(seriesKey), _.get(xAxis.key).map(_.toDouble), _.get(yAxis.key).map(_.toDouble))
      val sortedContents = contents.toIndexedSeq.sortBy(_._1)(OrderingForStringWithNumbers.SpecialDotTreatment)
      val data = sortedContents.map { case (plot, map) =>
        val series = new YIntervalSeries(plot)
        for ((x, y) <- map.toIndexedSeq.sortBy(_._1.toInt)) {
          val ySorted = y.toIndexedSeq.sorted
          series.add(x, ySorted(ySorted.size / 2), ySorted.head, ySorted.last)
        }
        series
      }

      val jFreeData = new YIntervalSeriesCollection
      for (series <- data) {
        jFreeData.addSeries(series)
      }

      val xyPlot = new XYPlot(jFreeData, xAxis.toJFreeChartAxis, yAxis.toJFreeChartAxis, new CustomDeviationRenderer)
      xyPlot.setOrientation(PlotOrientation.VERTICAL)
      makeUpPlot(xyPlot)

      val (table, sorter) = prepareTable(data)
      LeafDescription(xyPlot, table, sorter)
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

  private class CustomDeviationRenderer extends DeviationRenderer {
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
