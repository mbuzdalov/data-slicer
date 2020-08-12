package ru.ifmo.ds.gui.async.node

import javax.swing.SwingUtilities
import ru.ifmo.ds.gui.async.LoggingListener

class LoggingWorkload(listener: LoggingListener) extends Workload {
  override type MainInput = Unit
  override type MainOutput = Unit
  override def onError(th: Throwable): Unit = listener.workloadError(this, th)
  override def beforeMain(): Unit = listener.workloadBefore(this)
  override def main(input: Unit): Unit = SwingUtilities.invokeLater(() => listener.workloadMain(this))
  override def afterMain(input: Unit): Unit = listener.workloadAfter(this)
}
