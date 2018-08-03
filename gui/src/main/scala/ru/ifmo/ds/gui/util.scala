package ru.ifmo.ds.gui

import java.util.concurrent.{Executors, Future, FutureTask}

import javax.swing.SwingUtilities

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
}
