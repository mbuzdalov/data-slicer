package com.github.mbuzdalov.swingasync

import java.util.concurrent.ConcurrentLinkedQueue

import javax.swing.SwingUtilities

import com.github.mbuzdalov.swingasync.node.{Node, NodeListener, Workload}

class LoggingListener extends NodeListener {
  require(SwingUtilities.isEventDispatchThread)

  import LoggingListener._
  private[this] val records = new ConcurrentLinkedQueue[LogRecord]()

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

  def workloadBefore(workload: Workload): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    records.add(WorkBefore(workload))
  }

  def workloadMain(workload: Workload): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    records.add(WorkMain(workload))
  }

  def workloadAfter(workload: Workload): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    records.add(WorkAfter(workload))
  }

  def workloadError(workload: Workload, throwable: Throwable): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    records.add(WorkError(workload, throwable))
  }

  override def nodeJustAdded(node: Node, state: Node.State): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    records.add(Add(node, state))
  }

  override def stateChanged(node: Node, oldState: Node.State, newState: Node.State): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    records.add(Change(node, oldState, newState))
  }

  override def nodeJustRemoved(node: Node, state: Node.State): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    records.add(Remove(node, state))
  }
}

object LoggingListener {
  sealed trait LogRecord
  case class Add(node: Node, state: Node.State) extends LogRecord
  case class Change(node: Node, oldState: Node.State, newState: Node.State) extends LogRecord
  case class Remove(node: Node, state: Node.State) extends LogRecord
  case class WorkBefore(workload: Workload) extends LogRecord
  case class WorkMain(workload: Workload) extends LogRecord
  case class WorkAfter(workload: Workload) extends LogRecord
  case class WorkError(workload: Workload, throwable: Throwable) extends LogRecord
}
