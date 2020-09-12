package com.github.mbuzdalov.swingasync.ng.testutil

import java.util.concurrent.CountDownLatch

import javax.swing.SwingUtilities

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.github.mbuzdalov.swingasync.Infrastructure.inSwing
import com.github.mbuzdalov.swingasync.ng.UpdatableValue
import com.github.mbuzdalov.swingasync.ng.UpdatableValue.{Done, Failed, State, Waiting}

trait CommonTesting extends AnyFlatSpec with Matchers {
  protected class ConstantUpdatableValue[+A](override val state: State[A]) extends UpdatableValue[A]
  protected def done[A](value: A): UpdatableValue[A] = new ConstantUpdatableValue[A](Done(value))
  protected def waiting[A]: UpdatableValue[A] = new ConstantUpdatableValue[A](Waiting)
  protected def failed[A](th: Throwable): UpdatableValue[A] = new ConstantUpdatableValue[A](Failed(th))

  protected def check[T](expected: T, actual: UpdatableValue[T]): Unit = {
    require(!SwingUtilities.isEventDispatchThread)
    val latch = new CountDownLatch(1)
    val listener: UpdatableValue.Listener[T] = new UpdatableValue.Listener[T] {
      override def addedToValue(value: UpdatableValue[T]): Unit = latchOn(value)
      override def valueChanged(value: UpdatableValue[T], oldState: State[T]): Unit = latchOn(value)
      override def removedFromValue(value: UpdatableValue[T]): Unit = {}
      private def latchOn(value: UpdatableValue[T]): Unit = value.state match {
        case UpdatableValue.Done(_) => latch.countDown()
        case _ =>
      }
    }

    SwingUtilities.invokeAndWait(() => actual.addListener(listener))
    latch.await()
    SwingUtilities.invokeAndWait(() => actual.removeListener(listener))
    inSwing(actual.state shouldEqual UpdatableValue.Done(expected))
    inSwing(actual.state shouldEqual UpdatableValue.Done(expected))
  }
}
