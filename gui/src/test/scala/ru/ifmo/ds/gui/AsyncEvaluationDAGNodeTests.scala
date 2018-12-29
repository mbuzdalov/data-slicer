package ru.ifmo.ds.gui

import java.util.concurrent.atomic.AtomicReference

import javax.swing.SwingUtilities
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class AsyncEvaluationDAGNodeTests extends FlatSpec with Matchers {
  import AsyncEvaluationDAGNodeTests._

  "GUI node without dependencies that does not fail" should "behave as expected on creation" in {
    val logger = new Logger
    val seq = new ExpectedSequence("+!", "Ei", "01", "Ek", "13")
    SwingUtilities.invokeLater(() => new SimpleNodeThatCanFail(Seq.empty, "Test", logger, false))
    seq.run(logger, -1, 1000000)
  }

  it should "behave as expected on reloading" in {
    val logger = new Logger
    val seq1 = new ExpectedSequence("+!", "Ei", "01", "Ek", "13")
    val node = new AtomicReference[SimpleNodeThatCanFail]()
    SwingUtilities.invokeLater(() => node.set(new SimpleNodeThatCanFail(Seq.empty, "Test", logger, false)))
    seq1.run(logger, -1, 1000000)
    node.get() shouldNot be(null)

    logger.reset()
    val seq2 = new ExpectedSequence("R", "30", "Ei", "01", "Ek", "13")
    SwingUtilities.invokeLater(() => node.get().initiateReloading())
    seq2.run(logger, -1, 1000000)
  }

  "GUI node without dependencies that fails" should "behave as expected on creation" in {
    val logger = new Logger
    val seq = new ExpectedSequence("+!", "Ei", "01", "Ef", "10")
    SwingUtilities.invokeLater(() => new SimpleNodeThatCanFail(Seq.empty, "Test", logger, true))
    seq.run(logger, -1, 1000000)
  }
}

object AsyncEvaluationDAGNodeTests {
  import AsyncEvaluationDAGNode._

  class ExpectedSequence(actions: String*) {
    private[this] val prefixes = actions.scan("")(_ + _).zipWithIndex.toMap
    @tailrec
    final def run(logger: Logger, index: Int, moreIterations: Int): Unit = if (index == prefixes.size - 1) {
      if (moreIterations == 0) {
        // ok done
      } else run(logger, index, moreIterations - 1)
    } else {
      val str = logger.get
      prefixes.get(str) match {
        case None =>
          throw new AssertionError(s"Unexpected situation: $str. Action sequence is ${actions.mkString("", ", ", "")}")
        case Some(v) =>
          if (v < index) {
            throw new AssertionError(s"Unexpected fallback: index was $index now $v. Action sequence is ${actions.mkString("", ", ", "")}")
          } else {
            run(logger, v, moreIterations)
          }
      }
    }
  }

  class FailByDesignException extends Exception
  val failByDesignException = new FailByDesignException

  class SimpleNodeThatCanFail(inputs: Seq[AsyncEvaluationDAGNode], name: String, watcher: Watcher, willFail: Boolean)
    extends AsyncEvaluationDAGNode(inputs, name, watcher) {
    override protected def runEvaluation(): Unit = if (willFail) throw failByDesignException
  }

  class Logger extends Watcher {
    // We use StringBuffer intentionally, as accesses can be done from different threads
    private val builder = new StringBuffer()

    def get: String = builder.toString
    def reset(): Unit = builder.setLength(0)

    override def created(node: AsyncEvaluationDAGNode, waitsForDependencies: Boolean): Unit = {
      builder.append(if (waitsForDependencies) "+?" else "+!")
    }
    override def stateTransition(node: AsyncEvaluationDAGNode, oldState: State, newState: State): Unit = {
      builder.append(s"${oldState.id}${newState.id}")
    }
    override def initiateEvaluation(node: AsyncEvaluationDAGNode): Unit = builder.append("Ei")
    override def notifyParentIsNotEvaluated(node: AsyncEvaluationDAGNode): Unit = builder.append("Pn")
    override def notifyParentIsEvaluated(node: AsyncEvaluationDAGNode): Unit = builder.append("Pe")
    override def notifyEvaluationCrashed(node: AsyncEvaluationDAGNode): Unit = builder.append("Ef")
    override def notifyEvaluationComplete(node: AsyncEvaluationDAGNode): Unit = builder.append("Ek")
    override def initiateReloading(node: AsyncEvaluationDAGNode): Unit = builder.append("R")
  }
}
