package com.github.mbuzdalov.swingasync.node

import com.github.mbuzdalov.swingasync.LoggingListener
import javax.swing.SwingUtilities

class HeavyLoggingWorkload(listener: LoggingListener) extends Workload {
  override type MainInput = Unit
  override type MainOutput = Unit
  override def onError(th: Throwable): Unit = listener.workloadError(this, th)
  override def beforeMain(): Workload.HeavyResult[Unit] = Workload.HeavyResult(listener.workloadBefore(this))
  override def main(input: Unit): Unit = SwingUtilities.invokeLater(() => listener.workloadMain(this))
  override def afterMain(input: Unit): Unit = listener.workloadAfter(this)
}