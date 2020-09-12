package com.github.mbuzdalov.swingasync.ng.testutil

import java.util.concurrent.ConcurrentLinkedQueue

import com.github.mbuzdalov.swingasync.ng.UpdatableValue
import javax.swing.SwingUtilities

class LoggingListener[-A] extends UpdatableValue.Listener[A] {
  import LoggingListener._

  private[this] val records = new ConcurrentLinkedQueue[LogRecord]()

  override def addedToValue(value: UpdatableValue[A]): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    records.add(Add(value, value.state))
  }

  override def valueChanged(value: UpdatableValue[A], oldState: UpdatableValue.State[A]): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    records.add(Change(value, oldState, value.state))
  }

  override def removedFromValue(value: UpdatableValue[A]): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    records.add(Remove(value, value.state))
  }

  @scala.annotation.tailrec
  private[this] def pollUntilTimeLimit(remaining: Int): LogRecord = {
    if (remaining == 0) null else {
      val element = records.poll()
      if (element != null) element else {
        Thread.sleep(1)
        pollUntilTimeLimit(remaining - 1)
      }
    }
  }

  def failIfSomethingHappens(timeoutMilliseconds: Int): Unit = {
    Thread.sleep(timeoutMilliseconds)
    val next = records.poll()
    if (next != null) {
      throw new AssertionError("Expected nothing in the queue, found " + next)
    }
  }

  def consumeOrFail(seq: LogRecord*): Unit = {
    require(!SwingUtilities.isEventDispatchThread)
    var idx = 0
    while (idx < seq.size) {
      val next = seq(idx)
      val actual = pollUntilTimeLimit(1000)
      if (actual == null) {
        throw new AssertionError("Expected " + next + ", nothing found at index " + idx)
      }
      if (next != actual) {
        throw new AssertionError("Expected " + next + " found " + actual + " at index " + idx)
      }
      idx += 1
    }
  }
}

object LoggingListener {
  sealed trait LogRecord
  case class Add[A](node: UpdatableValue[A], state: UpdatableValue.State[A]) extends LogRecord
  case class Change[A](node: UpdatableValue[A], oldState: UpdatableValue.State[A], newState: UpdatableValue.State[A]) extends LogRecord
  case class Remove[A](node: UpdatableValue[A], state: UpdatableValue.State[A]) extends LogRecord
}
