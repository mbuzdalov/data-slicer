package ru.ifmo.ds.gui.async.value

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ru.ifmo.ds.gui.async.Infrastructure._

class SwingValueTests extends AnyFlatSpec with Matchers {
  private[this] def check[T](expected: T, actual: SwingValue[T]): Unit = {
    actual.busyWaitUntilComplete()
    inSwing(actual.value) shouldEqual expected
    inSwing(actual.value) shouldEqual expected
  }

  "The constant SwingValue" should "return its constant" in {
    for (_ <- 0 until 10) {
      val cv = inSwing(SwingValue.constant(5))
      check(5, cv)
    }
  }

  "The one-arg binding of SwingValue" should "compute the function correctly" in {
    for (_ <- 0 until 10) {
      val five = inSwing(SwingValue.constant(5))
      val fiveStr = inSwing(SwingValue.bind(five)(_.toString))
      check("5", fiveStr)
    }
  }

  "The two-arg binding of SwingValue" should "compute the function correctly" in {
    for (_ <- 0 until 10) {
      val a, b = inSwing(SwingValue.constant(5))
      val c = inSwing(SwingValue.bind(a, b)(_ + _))
      check(10, c)
    }
  }

  "The three-arg binding of SwingValue" should "compute the function correctly" in {
    for (_ <- 0 until 10) {
      val str = inSwing(SwingValue.constant("test string"))
      val suffix = inSwing(SwingValue.constant(5))
      val index = inSwing(SwingValue.constant(3))
      val tested = inSwing(SwingValue.bind(str, suffix, index)(_.substring(_).charAt(_)))
      check('i', tested)
    }
  }

  "The sequenced binding of SwingValue" should "compute the function correctly" in {
    for (_ <- 0 until 10) {
      val inputs = inSwing(IndexedSeq.tabulate(10)(SwingValue.constant))
      val sum = inSwing(SwingValue.bind(inputs: _*)(_.sum))
      check(45, sum)
    }
  }
}
