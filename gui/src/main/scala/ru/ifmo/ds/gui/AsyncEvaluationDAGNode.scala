package ru.ifmo.ds.gui

import javax.swing.SwingUtilities

import scala.collection.{Set => GeneralSet}
import scala.collection.mutable

abstract class AsyncEvaluationDAGNode(inputs: Seq[AsyncEvaluationDAGNode]) {
  ensureInSwing()

  import AsyncEvaluationDAGNode._

  private val children = new mutable.HashSet[AsyncEvaluationDAGNode]
  private[this] var _state: State = NotEvaluated
  private var nInputsWaitedFor = inputs.count(_.state != Evaluated)

  private def state: State = _state
  //noinspection ScalaUnusedSymbol: the plugin highlights the usages but does not count them somehow
  private def state_=(newState: State): Unit = {
    if (DEBUG) println(toString + ": " + _state + " => " + newState)
    _state = newState
  }

  inputs.foreach(_.children += this)

  if (nInputsWaitedFor == 0) {
    initiateEvaluation()
  } else {
    notifyWaitingForDependencies()
  }

  private def initiateEvaluation(): Unit = {
    ensureInSwing()
    assert(state == NotEvaluated)
    assert(nInputsWaitedFor == 0)
    if (DEBUG) println(toString + ".initiateEvaluation() called")
    state = Evaluating
    notifyEvaluationStarting()
    new Thread(() => {
      runEvaluation()
      SwingUtilities.invokeLater(() => {
        notifyEvaluationFinished()
        notifyEvaluationComplete()
      })
    }).start()
  }

  //noinspection ScalaUnusedSymbol: in fact, used in this method just below
  private def notifyParentIsNotEvaluated(): Unit = {
    ensureInSwing()
    assert(nInputsWaitedFor < inputs.size)
    if (DEBUG) println(toString + ".notifyParentIsNotEvaluated() called")
    if (nInputsWaitedFor == 0) {
      state match {
        case Evaluated =>
          state = NotEvaluated
          notifyWaitingForDependencies()
          children.foreach(_.notifyParentIsNotEvaluated())
        case Evaluating =>
          state = EvaluatingWhileReceivingRequest
        case NotEvaluated | EvaluatingWhileReceivingRequest =>
          // both legal, do nothing
      }
    } else {
      assert(state == NotEvaluated)
    }
    nInputsWaitedFor += 1
  }

  private def notifyParentIsEvaluated(): Unit = {
    ensureInSwing()
    assert(nInputsWaitedFor > 0)
    if (DEBUG) println(toString + ".notifyParentIsEvaluated() called")
    nInputsWaitedFor -= 1
    if (nInputsWaitedFor == 0) {
      state match {
        case Evaluated =>
          throw new AssertionError("Illegal state: Evaluated")
        case NotEvaluated =>
          initiateEvaluation()
        case Evaluating | EvaluatingWhileReceivingRequest =>
          state = EvaluatingWhileReceivingRequest
      }
    } else {
      assert(state == NotEvaluated || state == EvaluatingWhileReceivingRequest)
    }
  }

  private def notifyEvaluationComplete(): Unit = {
    ensureInSwing()
    if (DEBUG) println(toString + ".notifyEvaluationComplete() called")
    state match {
      case NotEvaluated =>
        throw new AssertionError("Illegal state: NotEvaluated")
      case Evaluated =>
        throw new AssertionError("Illegal state: Evaluated")
      case EvaluatingWhileReceivingRequest =>
        state = NotEvaluated
        if (nInputsWaitedFor == 0) {
          initiateEvaluation()
        } else {
          notifyWaitingForDependencies()
        }
      case Evaluating =>
        state = Evaluated
        children.foreach(_.notifyParentIsEvaluated())
    }
  }

  protected def notifyWaitingForDependencies(): Unit = {} /* will be called in Swing */
  protected def notifyEvaluationStarting(): Unit = {} /* will be called in Swing */
  protected def notifyEvaluationFinished(): Unit = {} /* will be called in Swing */

  protected def getChildren: GeneralSet[AsyncEvaluationDAGNode] = children
  protected def runEvaluation(): Unit /* will be called in a dedicated thread */
  protected def initiateReloading(): Unit = {
    ensureInSwing()
    if (DEBUG) println(toString + ".initiateReloading() called")
    state match {
      case NotEvaluated =>
        // do nothing
      case Evaluated =>
        state = NotEvaluated
        children.foreach(_.notifyParentIsNotEvaluated())
        initiateEvaluation()
      case Evaluating | EvaluatingWhileReceivingRequest =>
        state = EvaluatingWhileReceivingRequest
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
  private final val DEBUG = false

  private sealed trait State
  private case object NotEvaluated extends State
  private case object Evaluating extends State
  private case object EvaluatingWhileReceivingRequest extends State
  private case object Evaluated extends State
}
