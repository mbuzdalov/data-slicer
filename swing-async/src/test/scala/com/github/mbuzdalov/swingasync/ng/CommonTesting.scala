package com.github.mbuzdalov.swingasync.ng

import com.github.mbuzdalov.swingasync.ng.UpdatableValue.{Done, Failed, State, Waiting}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait CommonTesting extends AnyFlatSpec with Matchers {
  protected class ConstantUpdatableValue[+A](override val state: State[A]) extends UpdatableValue[A]
  protected def done[A](value: A): UpdatableValue[A] = new ConstantUpdatableValue[A](Done(value))
  protected def waiting[A]: UpdatableValue[A] = new ConstantUpdatableValue[A](Waiting)
  protected def failed[A](th: Throwable): UpdatableValue[A] = new ConstantUpdatableValue[A](Failed(th))
}
