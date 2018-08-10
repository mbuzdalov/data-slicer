package ru.ifmo.ds.gui

import org.jfree.chart.axis.{LogarithmicAxis, NumberAxis}

case class Axis(name: String, key: String, isLogarithmic: Boolean) {
  def toJFreeChartAxis: NumberAxis = if (isLogarithmic) {
    new LogarithmicAxis(name)
  } else {
    new NumberAxis(name)
  }
}
