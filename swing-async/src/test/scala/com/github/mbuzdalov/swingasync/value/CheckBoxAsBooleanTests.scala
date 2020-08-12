package com.github.mbuzdalov.swingasync.value

import com.github.mbuzdalov.swingasync.Infrastructure.inSwing
import com.github.mbuzdalov.swingasync.LoggingListener
import com.github.mbuzdalov.swingasync.node.Node
import com.github.mbuzdalov.swingasync.value.components.CheckBoxAsBoolean

class CheckBoxAsBooleanTests extends ExtendedSpec {
  "The initial value" should "be false" in {
    for (_ <- 0 until 10) {
      val cv = inSwing(new CheckBoxAsBoolean)
      check(false, cv)
    }
  }

  "Running doClick on the checkbox" should "toggle the SwingValue" in {
    for (_ <- 0 until 5) {
      val cv = inSwing(new CheckBoxAsBoolean)
      inSwing(cv.component.doClick(10))
      check(true, cv)
      inSwing(cv.component.doClick(10))
      check(false, cv)
      inSwing(cv.component.doClick(10))
      check(true, cv)
      inSwing(cv.component.doClick(10))
      check(false, cv)
    }
  }

  "Running setSelected on the checkbox" should "set the SwingValue to that value" in {
    for (_ <- 0 until 10) {
      val cv = inSwing(new CheckBoxAsBoolean)
      inSwing(cv.component.setSelected(true))
      check(true, cv)
      inSwing(cv.component.setSelected(true))
      check(true, cv)
      inSwing(cv.component.setSelected(false))
      check(false, cv)
      inSwing(cv.component.setSelected(true))
      check(true, cv)
    }
  }

  it should "not notify the listeners if the same value is set" in {
    for (_ <- 0 until 5) {
      import LoggingListener._
      import Node._

      val cv = inSwing(new CheckBoxAsBoolean)
      val cvNode = inSwing(cv.node)
      val listener = inSwing(new LoggingListener())
      inSwing(cvNode.addListener(listener))
      listener.consumeOrFail(Add(cvNode, Done))
      inSwing(cv.component.setSelected(true))
      listener.consumeOrFail(Change(cvNode, Done, Waiting), Change(cvNode, Waiting, Done))
      check(true, cv)
      inSwing(cv.component.setSelected(true))
      listener.failIfSomethingHappens(20)
      check(true, cv)
      inSwing(cv.component.setSelected(false))
      listener.consumeOrFail(Change(cvNode, Done, Waiting), Change(cvNode, Waiting, Done))
      check(false, cv)
      inSwing(cv.component.setSelected(false))
      listener.failIfSomethingHappens(20)
      check(false, cv)
      inSwing(cv.component.setSelected(true))
      listener.consumeOrFail(Change(cvNode, Done, Waiting), Change(cvNode, Waiting, Done))
      check(true, cv)
    }
  }
}
