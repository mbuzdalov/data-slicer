package ru.ifmo.ds.gui

import javax.swing.SwingUtilities

import scala.collection.Set
import scala.collection.mutable

abstract class AsyncEvaluationDAGNode private[gui] (inputs: Seq[AsyncEvaluationDAGNode],
                                                    private[gui] val name: String,
                                                    watcher: AsyncEvaluationDAGNode.Watcher) {
  def this(inputs: Seq[AsyncEvaluationDAGNode], name: String) = {
    this(inputs, name, AsyncEvaluationDAGNode.defaultWatcher)
  }

  ensureInSwing()

  import AsyncEvaluationDAGNode._

  private val children = new mutable.HashSet[AsyncEvaluationDAGNode]
  private[this] var state: State = NotEvaluated
  private[this] var nInputsWaitedFor = inputs.count(_.getState != Evaluated)

  private def getState: State = state

  private[this] def setState(newState: State): Unit = {
    if (state != newState) {
      val oldState = state
      state = newState
      watcher.stateTransition(this, oldState, newState)
    }
  }

  inputs.foreach(_.children += this)

  watcher.created(this, nInputsWaitedFor != 0)

  // invokeLater is used here to fire the methods strictly after the constructor is completed
  if (nInputsWaitedFor == 0) {
    SwingUtilities.invokeLater(() => initiateEvaluation())
  } else {
    SwingUtilities.invokeLater(() => notifyWaitingForDependencies())
  }

  private def initiateEvaluation(): Unit = {
    ensureInSwing()
    assert(state == NotEvaluated)
    assert(nInputsWaitedFor == 0)
    watcher.initiateEvaluation(this)
    setState(Evaluating)
    notifyEvaluationStarting()
    new Thread(() => {
      try {
        runEvaluation()
        SwingUtilities.invokeLater(() => {
          notifyEvaluationFinished()
          onEvaluationComplete()
        })
      } catch {
        case th: Throwable =>
          SwingUtilities.invokeLater(() => {
            notifyEvaluationFailed(th)
            onEvaluationCrashed()
          })
      }
    }).start()
  }

  private def onParentIsNotEvaluated(): Unit = {
    ensureInSwing()
    assert(nInputsWaitedFor < inputs.size)
    watcher.notifyParentIsNotEvaluated(this)
    if (nInputsWaitedFor == 0) {
      state match {
        case Evaluated =>
          setState(NotEvaluated)
          notifyWaitingForDependencies()
          children.foreach(_.onParentIsNotEvaluated())
        case Evaluating =>
          setState(EvaluatingWhileReceivingRequest)
        case NotEvaluated | EvaluatingWhileReceivingRequest =>
          // both legal, do nothing
      }
    } else {
      assert(state == NotEvaluated)
    }
    nInputsWaitedFor += 1
  }

  private def onParentIsEvaluated(): Unit = {
    ensureInSwing()
    assert(nInputsWaitedFor > 0)
    watcher.notifyParentIsEvaluated(this)
    nInputsWaitedFor -= 1
    if (nInputsWaitedFor == 0) {
      state match {
        case Evaluated =>
          throw new AssertionError("Illegal state: Evaluated")
        case NotEvaluated =>
          initiateEvaluation()
        case Evaluating | EvaluatingWhileReceivingRequest =>
          setState(EvaluatingWhileReceivingRequest)
      }
    } else {
      assert(state == NotEvaluated || state == EvaluatingWhileReceivingRequest)
    }
  }

  private def onEvaluationCrashed(): Unit = {
    ensureInSwing()
    watcher.notifyEvaluationCrashed(this)
    state match {
      case NotEvaluated =>
        throw new AssertionError("Illegal state: NotEvaluated")
      case Evaluated =>
        throw new AssertionError("Illegal state: Evaluated")
      case EvaluatingWhileReceivingRequest =>
        setState(NotEvaluated)
        if (nInputsWaitedFor == 0) {
          initiateEvaluation()
        } else {
          notifyWaitingForDependencies()
        }
      case Evaluating =>
        setState(NotEvaluated)
    }
  }

  private def onEvaluationComplete(): Unit = {
    ensureInSwing()
    watcher.notifyEvaluationComplete(this)
    state match {
      case NotEvaluated =>
        throw new AssertionError("Illegal state: NotEvaluated")
      case Evaluated =>
        throw new AssertionError("Illegal state: Evaluated")
      case EvaluatingWhileReceivingRequest =>
        setState(NotEvaluated)
        if (nInputsWaitedFor == 0) {
          initiateEvaluation()
        } else {
          notifyWaitingForDependencies()
        }
      case Evaluating =>
        setState(Evaluated)
        children.foreach(_.onParentIsEvaluated())
    }
  }

  protected def notifyWaitingForDependencies(): Unit = {} /* will be called in Swing */
  protected def notifyEvaluationStarting(): Unit = {} /* will be called in Swing */
  protected def notifyEvaluationFinished(): Unit = {} /* will be called in Swing */
  protected def notifyEvaluationFailed(throwable: Throwable): Unit = {} /* will be called in Swing */

  protected def getChildren: Set[AsyncEvaluationDAGNode] = children
  protected def runEvaluation(): Unit /* will be called in a dedicated thread */

  def initiateReloading(): Unit = {
    ensureInSwing()
    watcher.initiateReloading(this)
    state match {
      case NotEvaluated =>
        if (nInputsWaitedFor == 0) {
          initiateEvaluation()
        } else {
          notifyWaitingForDependencies()
        }
      case Evaluated =>
        setState(NotEvaluated)
        children.foreach(_.onParentIsNotEvaluated())
        initiateEvaluation()
      case Evaluating | EvaluatingWhileReceivingRequest =>
        setState(EvaluatingWhileReceivingRequest)
    }
  }
  protected def disconnectFromInputs(): Unit = {
    inputs.foreach(_.children -= this)
  }

  private def ensureInSwing(): Unit = {
    assert(SwingUtilities.isEventDispatchThread,
           "Internals of AsyncEvaluationDAGNode must be addressed from the Swing thread")
  }
}

object AsyncEvaluationDAGNode {
  private[gui] trait Watcher {
    def created(node: AsyncEvaluationDAGNode, waitsForDependencies: Boolean): Unit
    def stateTransition(node: AsyncEvaluationDAGNode, oldState: State, newState: State): Unit
    def initiateEvaluation(node: AsyncEvaluationDAGNode): Unit
    def notifyParentIsNotEvaluated(node: AsyncEvaluationDAGNode): Unit
    def notifyParentIsEvaluated(node: AsyncEvaluationDAGNode): Unit
    def notifyEvaluationCrashed(node: AsyncEvaluationDAGNode): Unit
    def notifyEvaluationComplete(node: AsyncEvaluationDAGNode): Unit
    def initiateReloading(node: AsyncEvaluationDAGNode): Unit
  }

  private val emptyWatcher = new Watcher {
    override def created(node: AsyncEvaluationDAGNode, waitsForDependencies: Boolean): Unit = {}
    override def stateTransition(node: AsyncEvaluationDAGNode, oldState: State, newState: State): Unit = {}
    override def initiateEvaluation(node: AsyncEvaluationDAGNode): Unit = {}
    override def notifyParentIsNotEvaluated(node: AsyncEvaluationDAGNode): Unit = {}
    override def notifyParentIsEvaluated(node: AsyncEvaluationDAGNode): Unit = {}
    override def notifyEvaluationCrashed(node: AsyncEvaluationDAGNode): Unit = {}
    override def notifyEvaluationComplete(node: AsyncEvaluationDAGNode): Unit = {}
    override def initiateReloading(node: AsyncEvaluationDAGNode): Unit = {}
  }

  private val stdoutWatcher: Watcher = new Watcher {
    private def dump(node: AsyncEvaluationDAGNode): String = s"Node[${node.hashCode().toHexString}]"
    private def dumpWide(node: AsyncEvaluationDAGNode): String = s"Node[${node.hashCode().toHexString}] = ${node.name}"
    override def created(node: AsyncEvaluationDAGNode, waitsForDependencies: Boolean): Unit = {
      println(s"${dumpWide(node)} created, ${if (waitsForDependencies) "waits for dependencies" else "has all dependencies resolved"}")
    }
    override def stateTransition(node: AsyncEvaluationDAGNode, oldState: State, newState: State): Unit = {
      println(s"${dump(node)}: $oldState => $newState")
    }
    override def initiateEvaluation(node: AsyncEvaluationDAGNode): Unit = {
      println(s"${dump(node)}.initiateEvaluation() called")
    }
    override def notifyParentIsNotEvaluated(node: AsyncEvaluationDAGNode): Unit = {
      println(s"${dump(node)}.onParentIsNotEvaluated() called")
    }
    override def notifyParentIsEvaluated(node: AsyncEvaluationDAGNode): Unit = {
      println(s"${dump(node)}.onParentIsEvaluated() called")
    }
    override def notifyEvaluationCrashed(node: AsyncEvaluationDAGNode): Unit = {
      println(s"${dump(node)}.onEvaluationCrashed() called")
    }
    override def notifyEvaluationComplete(node: AsyncEvaluationDAGNode): Unit = {
      println(s"${dump(node)}.onEvaluationComplete() called")
    }
    override def initiateReloading(node: AsyncEvaluationDAGNode): Unit = {
      println(s"${dump(node)}.initiateReloading() called")
    }
  }

  // With -Dslicer.gui.node.debug=true, all nodes will dump their changes to stdout
  private val defaultWatcher = {
    if (System.getProperty("slicer.gui.node.debug", "false") == "true") stdoutWatcher else emptyWatcher
  }

  private[gui] sealed abstract class State(val id: Int)
  private[gui] case object NotEvaluated extends State(0)
  private[gui] case object Evaluating extends State(1)
  private[gui] case object EvaluatingWhileReceivingRequest extends State(2)
  private[gui] case object Evaluated extends State(3)
}
