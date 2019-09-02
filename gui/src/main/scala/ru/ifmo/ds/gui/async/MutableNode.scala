package ru.ifmo.ds.gui.async

import javax.swing.SwingUtilities

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

import ru.ifmo.ds.gui.async.Node._

final class MutableNode(workload: Workload) extends Node with NodeListener {
  private[this] var state: State = Done
  private[this] var nDependenciesToWait = 0
  private[this] val dependencies = new mutable.HashSet[Node]()

  override protected def getState: State = state

  def addDependency(dependency: Node): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    if (!dependencies.contains(dependency)) {
      dependencies += dependency
      dependency.addListener(this)
    }
  }

  def removeDependency(dependency: Node): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    if (dependencies.contains(dependency)) {
      dependency.removeListener(this)
      dependencies -= dependency
    }
  }

  override def nodeJustAdded(node: Node, state: State): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    require(dependencies.contains(node))
    processDependencyChange(1, if (state == Done) 0 else 1)
  }

  override def stateChanged(node: Node, oldState: State, newState: State): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    require(dependencies.contains(node))
    require(oldState != newState)
    processDependencyChange(0, if (oldState == Done) 1 else if (newState == Done) -1 else 0)
  }

  override def nodeJustRemoved(node: Node, state: State): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    require(dependencies.contains(node))
    processDependencyChange(-1, if (state == Done) 0 else -1)
  }

  private def checkInvariants(): Unit = {
    state match {
      case Waiting => assert(nDependenciesToWait > 0)
      case Restarting => assert(nDependenciesToWait >= 0)
      case Running | Done | Failed => assert(nDependenciesToWait == 0)
    }
  }

  private def processDependencyChange(nDependenciesChange: Int, nWaitingChange: Int): Unit = {
    checkInvariants()
    if (nDependenciesChange != 0 || nWaitingChange != 0) {
      val oldState = state
      nDependenciesToWait += nDependenciesChange
      state = state match {
        case Waiting | Failed | Done => if (nDependenciesToWait == 0) scheduleFunction() else Waiting
        case Running | Restarting    => Restarting
      }
      notifyOfStateChange(oldState)
      checkInvariants()
    }
  }

  private def notifyFunctionResult(result: Try[workload.MainOutput]): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    checkInvariants()
    val oldState = state
    state = state match {
      case Waiting | Failed | Done =>
        throw new AssertionError(s"State cannot be $state when function returns")
      case Running =>
        result match {
          case Failure(th) =>
            workload.onError(th)
            Failed
          case Success(result) =>
            try {
              workload.afterMain(result)
              Done
            } catch {
              case th: Throwable =>
                workload.onError(th)
                Failed
            }
        }
      case Restarting =>
        if (nDependenciesToWait == 0) scheduleFunction() else Waiting
    }
    notifyOfStateChange(oldState)
    checkInvariants()
  }

  private def scheduleFunction(): State = {
    try {
      val before = workload.beforeMain()
      new Thread(() => {
        val bTry = Try(workload.main(before))
        SwingUtilities.invokeLater(() => notifyFunctionResult(bTry))
      }).start()
      Running
    } catch {
      case th: Throwable =>
        workload.onError(th)
        Failed
    }
  }
}