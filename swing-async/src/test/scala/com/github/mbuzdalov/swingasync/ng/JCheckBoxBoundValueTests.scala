package com.github.mbuzdalov.swingasync.ng

import javax.swing.JCheckBox

import com.github.mbuzdalov.swingasync.Infrastructure.inSwing
import com.github.mbuzdalov.swingasync.ng.testutil.{CommonTesting, LoggingListener}

class JCheckBoxBoundValueTests extends CommonTesting {
  "The initial value" should "be false" in {
    for (_ <- 0 until 10) {
      val cb = inSwing(new JCheckBox())
      val cv = inSwing(Binding.fromJCheckBox(cb))
      check(false, cv)
    }
  }

  "Running doClick on the checkbox" should "toggle the SwingValue" in {
    for (_ <- 0 until 5) {
      val cb = inSwing(new JCheckBox())
      val cv = inSwing(Binding.fromJCheckBox(cb))
      inSwing(cb.doClick(10))
      check(true, cv)
      inSwing(cb.doClick(10))
      check(false, cv)
      inSwing(cb.doClick(10))
      check(true, cv)
      inSwing(cb.doClick(10))
      check(false, cv)
    }
  }

  "Running setSelected on the checkbox" should "set the SwingValue to that value" in {
    for (_ <- 0 until 10) {
      val cb = inSwing(new JCheckBox())
      val cv = inSwing(Binding.fromJCheckBox(cb))
      inSwing(cb.setSelected(true))
      check(true, cv)
      inSwing(cb.setSelected(true))
      check(true, cv)
      inSwing(cb.setSelected(false))
      check(false, cv)
      inSwing(cb.setSelected(true))
      check(true, cv)
    }
  }

  it should "not notify the listeners if the same value is set" in {
    for (_ <- 0 until 5) {
      import LoggingListener._
      import UpdatableValue._

      val cb = inSwing(new JCheckBox())
      val cv = inSwing(Binding.fromJCheckBox(cb))
      val listener = inSwing(new LoggingListener())
      inSwing(cv.addListener(listener))
      listener.consumeOrFail(Add(cv, Done(false)))
      inSwing(cb.setSelected(true))
      listener.consumeOrFail(Change(cv, Done(false), Done(true)))
      check(true, cv)
      inSwing(cb.setSelected(true))
      listener.failIfSomethingHappens(20)
      check(true, cv)
      inSwing(cb.setSelected(false))
      listener.consumeOrFail(Change(cv, Done(true), Done(false)))
      check(false, cv)
      inSwing(cb.setSelected(false))
      listener.failIfSomethingHappens(20)
      check(false, cv)
      inSwing(cb.setSelected(true))
      listener.consumeOrFail(Change(cv, Done(false), Done(true)))
      check(true, cv)
    }
  }
}
