package ru.ifmo.ds.gui.async.value

import java.util.concurrent.CountDownLatch

import javax.swing.SwingUtilities
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.ifmo.ds.gui.async.Infrastructure.inSwing
import ru.ifmo.ds.gui.async.node.{Node, NodeListener}

trait ExtendedSpec extends AnyFlatSpec with Matchers {
  def check[T](expected: T, actual: SwingValue[T]): Unit = {
    require(!SwingUtilities.isEventDispatchThread)
    val latch = new CountDownLatch(1)
    val listener = new NodeListener {
      override def nodeJustRemoved(node: Node, state: Node.State): Unit = {}
      override def nodeJustAdded(node: Node, state: Node.State): Unit = if (state == Node.Done) latch.countDown()
      override def stateChanged(node: Node, oldState: Node.State, newState: Node.State): Unit = {
        if (newState == Node.Done) latch.countDown()
      }
    }
    SwingUtilities.invokeAndWait(() => actual.node.addListener(listener))
    latch.await()
    SwingUtilities.invokeAndWait(() => actual.node.removeListener(listener))
    inSwing(actual.value) shouldEqual expected
    inSwing(actual.value) shouldEqual expected
  }
}
