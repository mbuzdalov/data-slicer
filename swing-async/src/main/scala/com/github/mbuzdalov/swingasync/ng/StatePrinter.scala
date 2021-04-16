package com.github.mbuzdalov.swingasync.ng

trait StatePrinter[-T] {
  def toString(state: UpdatableValue.State[T]): String
}

object StatePrinter {
  class ToStringOrText(onInitializing: String, onWaiting: String, onRunning: String,
                       onRestarting: String, onFailed: Throwable => String) extends StatePrinter[Any] {
    override def toString(state: UpdatableValue.State[Any]): String = state match {
      case UpdatableValue.Done(v) => v.toString
      case UpdatableValue.Waiting => onWaiting
      case UpdatableValue.Failed(th) => onFailed(th)
      case UpdatableValue.Running => onRunning
      case UpdatableValue.Restarting => onRestarting
      case UpdatableValue.Initializing => onInitializing
    }
  }

  val toStringOrShortText: StatePrinter[Any] =
    new ToStringOrText(onInitializing = "<init>", onWaiting = "<wait>", onRunning = "<run>",
      onRestarting = "<restart>", onFailed = _ => "<fail>")

  val toStringWithExceptionMessageOrShortText: StatePrinter[Any] =
    new ToStringOrText(onInitializing = "<init>", onWaiting = "<wait>", onRunning = "<run>",
      onRestarting = "<restart>", onFailed = _.getMessage)
}
