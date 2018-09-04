package ru.ifmo.ds.gui

import java.util.concurrent.{Executors, Future, FutureTask}

import javax.swing.SwingUtilities

import org.jfree.chart.axis.{LogarithmicAxis, NumberAxis}

import ru.ifmo.ds.util.Axis

object util {
  private[this] val nonGUIThread = Executors.newSingleThreadExecutor()

  def inSwing[T](fun: => T): Unit = {
    if (SwingUtilities.isEventDispatchThread) {
      fun
    } else {
      SwingUtilities.invokeLater(() => fun)
    }
  }

  def notInSwing[T](fun: => T): Future[T] = {
    if (SwingUtilities.isEventDispatchThread) {
      nonGUIThread.submit(() => fun)
    } else {
      val rv = new FutureTask[T](() => fun)
      rv.run()
      rv
    }
  }

  implicit class AxisExtension(val axis: Axis) extends AnyVal {
    def toJFreeChartAxis: NumberAxis = if (axis.isLogarithmic) {
      new LogarithmicAxis(axis.name)
    } else {
      new NumberAxis(axis.name)
    }
  }
}
