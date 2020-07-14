package ru.ifmo.ds.gui.async.node

import javax.swing.SwingUtilities
import scala.util.{Failure, Success, Try}
import ru.ifmo.ds.gui.async.node.Node._

final class MutableNode(workload: Workload) extends Node with NodeListener {
  private[this] var state: State = Initializing
  private[this] var nDependenciesToWait = 0

  override def getState: State = state

  def completeInitialization(): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    require(state == Initializing)
    if (nDependenciesToWait == 0) {
      state = scheduleFunction()
    } else {
      state = Waiting
    }
    notifyOfStateChange(Initializing)
  }

  def restartEvaluation(): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    checkInvariants()
    val oldState = state
    state = state match {
      case Initializing | Waiting | Restarting => state /* do nothing */
      case Running => Restarting /* if running, ask to be restarted */
      case Done | Failed => scheduleFunction() /* actually restart */
    }
    notifyOfStateChange(oldState)
  }

  override def nodeJustAdded(node: Node, state: State): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    processDependencyChange(1, if (state == Done) 0 else 1)
  }

  override def stateChanged(node: Node, oldState: State, newState: State): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    require(oldState != newState)
    processDependencyChange(0, if (oldState == Done) 1 else if (newState == Done) -1 else 0)
  }

  override def nodeJustRemoved(node: Node, state: State): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    processDependencyChange(-1, if (state == Done) 0 else -1)
  }

  private def checkInvariants(): Unit = {
    state match {
      case Waiting => assert(nDependenciesToWait > 0)
      case Initializing | Restarting => assert(nDependenciesToWait >= 0)
      case Running | Done | Failed => assert(nDependenciesToWait == 0)
    }
  }

  private def processDependencyChange(nDependenciesChange: Int, nWaitingChange: Int): Unit = {
    checkInvariants()
    if (nDependenciesChange != 0 || nWaitingChange != 0) {
      val oldState = state
      nDependenciesToWait += nWaitingChange
      state = state match {
        case Initializing => Initializing
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
      case Initializing | Waiting | Failed | Done =>
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
