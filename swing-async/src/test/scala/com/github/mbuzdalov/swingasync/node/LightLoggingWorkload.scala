package com.github.mbuzdalov.swingasync.node

import com.github.mbuzdalov.swingasync.LoggingListener

class LightLoggingWorkload(listener: LoggingListener) extends Workload {
  override type MainInput = Unit
  override type MainOutput = Unit
  override def onError(th: Throwable): Unit = listener.workloadError(this, th)
  override def beforeMain(): Workload.LightResult[Unit] = Workload.LightResult(listener.workloadBefore(this))
  override def main(input: Unit): Unit = throw new AssertionError("This must never be called")
  override def afterMain(input: Unit): Unit = listener.workloadAfter(this)
}
