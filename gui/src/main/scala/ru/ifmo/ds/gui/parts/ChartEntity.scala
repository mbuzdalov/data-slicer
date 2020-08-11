package ru.ifmo.ds.gui.parts

import java.awt.Color
import java.util.{Arrays, Comparator}

import javax.swing._
import javax.swing.event.TableModelListener
import javax.swing.table.{TableModel, TableRowSorter}

import scala.Ordering.Double.IeeeOrdering
import scala.collection.{mutable => mu}
import org.jfree.chart.plot.{PlotOrientation, XYPlot}
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.data.xy.{YIntervalSeries, YIntervalSeriesCollection}
import ru.ifmo.ds.Database
import ru.ifmo.ds.gui.util.JFreeUtils._
import ru.ifmo.ds.gui.util._
import ru.ifmo.ds.gui.{DisplayedEntity, EntityContainer}
import ru.ifmo.ds.gui.components.ManagedSplitter
import ru.ifmo.ds.util.{Axis, OrderingForStringWithNumbers}

class ChartEntity(inputs: Seq[DatabaseEntity], container: EntityContainer,
                  categoryKeys: Seq[String], seriesKey: String, xAxis: Axis, yAxis: Axis)
  extends DisplayedEntity(inputs, container, ChartEntity.chartIcon, inputs.view.map(_.getName).mkString("[", "+", "]")) {

  override def derive(newEntities: Seq[DisplayedEntity]): DisplayedEntity = {
    val newParents = newEntities.map(_.asInstanceOf[DatabaseEntity])
    new ChartEntity(newParents, container, categoryKeys, seriesKey, xAxis, yAxis)
  }

  override protected def runEvaluation(): Unit = {
    val dbs = Database.merge(inputs.map(_.getDatabase) :_*)
    val tree = ChartEntity.makeTree(dbs, categoryKeys, seriesKey, xAxis, yAxis)
    SwingUtilities.invokeAndWait(() => {
      theUI.removeAll()
      theUI.add(PartitionTreeUtils.createTabbedPanes[ChartEntity.LeafDescription](tree, _.makeUI()))
      theUI.revalidate()
      theUI.repaint()
    })
  }
}

object ChartEntity extends ImageLoadingFacilities {
  private val chartIcon = imageFromResource("chart.png")
  private val tableIcon = imageFromResource("table.png")
  private val chartTableVIcon = imageFromResource("chart-table-v.png")
  private val chartTableHIcon = imageFromResource("chart-table-h.png")

  private val minimumMedianColor = Color.GREEN
  private val fivePercentMedianColor = Color.CYAN
  private val tenPercentMedianColor = new Color(0xff, 0xff, 0x88)

  private case class LeafDescription(plot: XYPlot,
                                     tableModel: TableModel,
                                     tableRowSorter: TableRowSorter[TableModel]) {
    def makeUI(): JComponent = {
      val chartPanel = new ChartPanel(new JFreeChart(plot))
      JFreeUtils.augmentWithTogglingPlotsByClickingOnLegend(chartPanel)
      val table = new JTable(tableModel)
      table.setRowSorter(tableRowSorter)
      table.setDefaultRenderer(classOf[TableDoubleValueDisplay], TableDoubleValueDisplay.CellRenderer)
      TableUtils.alignTable(table)
      val tableScroll = new JScrollPane(table)
      new ManagedSplitter(chartPanel, chartIcon, "Show the chart only",
                          tableScroll, tableIcon, "Show the table only",
                          chartTableVIcon, chartTableHIcon)
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
    markupColumnMinima(tableContents)
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

  private def markupColumnMinima(table: Array[Array[AnyRef]]): Unit = {
    for (colIndex <- table(0).indices) {
      var bestMedian: Double = Double.NaN
      for (row <- table) {
        row(colIndex) match {
          case d: TableDoubleValueDisplay if !d.median.isNaN =>
            bestMedian = if (bestMedian.isNaN) d.median else math.min(d.median, bestMedian)
          case _ =>
        }
      }
      for (row <- table) {
        row(colIndex) match {
          case d: TableDoubleValueDisplay if !d.median.isNaN =>
            if (d.median <= bestMedian * 1.0001) {
              row(colIndex) = d.withColor(minimumMedianColor)
            } else if (d.median <= bestMedian * 1.05) {
              row(colIndex) = d.withColor(fivePercentMedianColor)
            } else if (d.median <= bestMedian * 1.1) {
              row(colIndex) = d.withColor(tenPercentMedianColor)
            }
          case _ =>
        }
      }
    }
  }

  private def makeLeafFromDatabase(seriesKey: String, xAxis: Axis, yAxis: Axis)(db: Database): LeafDescription = {
    val contents = db.groupMap2D(_.get(seriesKey), _.get(xAxis.key).map(_.toDouble), _.get(yAxis.key).map(_.toDouble))
    val sortedContents = contents.toIndexedSeq.sortBy(_._1)(OrderingForStringWithNumbers.SpecialDotTreatment)
    val data = sortedContents.map { case (plot, map) =>
      val series = new YIntervalSeries(plot)
      for ((x, y) <- map.toIndexedSeq.sortBy(_._1.toInt) if y.nonEmpty) {
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
    JFreeUtils.initializePlotColors(xyPlot)

    val (table, sorter) = prepareTable(data)
    LeafDescription(xyPlot, table, sorter)
  }

  private def makeTree(input: Database, keys: Seq[String], seriesKey: String, xAxis: Axis, yAxis: Axis) = {
    input.partitionByKeys(keys, makeLeafFromDatabase(seriesKey, xAxis, yAxis))
  }
}
