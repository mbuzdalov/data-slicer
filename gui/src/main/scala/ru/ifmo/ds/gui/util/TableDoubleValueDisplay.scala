package ru.ifmo.ds.gui.util

import java.awt.{Color, Component}
import java.util.Locale

import javax.swing.{JLabel, JTable}
import javax.swing.table.TableCellRenderer

class TableDoubleValueDisplay(val median: Double,
                              val min: Double,
                              val max: Double,
                              alwaysSci: Boolean,
                              color: Color = null) extends Comparable[TableDoubleValueDisplay] {
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

  def withColor(newColor: Color): TableDoubleValueDisplay =
    new TableDoubleValueDisplay(median, min, max, alwaysSci, newColor)

  def minText: String = constructValue(min)
  def maxText: String = constructValue(max)
  def medText: String = constructValue(median)

  override val toString: String =
    if (median.isNaN)
      "---"
    else if (color != null)
      s"<html><p style='background-color:#${(color.getRGB & 0xffffff).toHexString}'>" + constructValue(median) + "</p></html>"
    else
      "<html>" + constructValue(median) + "</html>"

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

object TableDoubleValueDisplay {
  final val NaN = new TableDoubleValueDisplay(Double.NaN, Double.NaN, Double.NaN, true)
  final val CellRenderer = new JLabel with TableCellRenderer {
    override def getTableCellRendererComponent(table: JTable, value: Any,
                                               isSelected: Boolean, hasFocus: Boolean,
                                               row: Int, column: Int): Component = {
      value match {
        case v: TableDoubleValueDisplay if !v.min.isNaN && !v.max.isNaN =>
          setText(v.toString)
          setToolTipText(s"<html>median = ${v.medText}<br/>min = ${v.minText}<br/>max = ${v.maxText}</html>")
          this
        case _ =>
          setText(value.toString)
          this
      }
    }
  }
}
