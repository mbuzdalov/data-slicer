package com.github.mbuzdalov.swingasync.ng

import javax.swing.JComboBox

import scala.reflect.ClassTag

import com.github.mbuzdalov.swingasync.Infrastructure.inSwing
import com.github.mbuzdalov.swingasync.ng.testutil.{CommonTesting, LoggingListener}

class JComboBoxBoundValueTests extends CommonTesting {
  import UpdatableValue._
  import LoggingListener._

  private def newComboBox[T <: AnyRef : ClassTag](values: T*): JComboBox[T] = new JComboBox[T](values.toArray)

  "The initial value" should "be Waiting for an empty box" in {
    val cb = inSwing(newComboBox[String]())
    val cv = inSwing(Binding.fromJComboBox(cb))
    val ls = inSwing(new LoggingListener[String])
    inSwing(cv.addListener(ls))
    ls.consumeOrFail(Add(cv, Waiting))
  }

  it should "be Done(first item) for an non-empty newly created box (as per JComboBox's documentation)" in {
    val cb = inSwing(newComboBox("a", "b", "c"))
    val cv = inSwing(Binding.fromJComboBox(cb))
    val ls = inSwing(new LoggingListener[String])
    inSwing(cv.addListener(ls))
    ls.consumeOrFail(Add(cv, Done("a")))
  }

  it should "be Waiting if the selection is explicitly erased" in {
    val cb = inSwing(newComboBox("a", "b", "c"))
    inSwing(cb.setSelectedItem(null))
    val cv = inSwing(Binding.fromJComboBox(cb))
    val ls = inSwing(new LoggingListener[String])
    inSwing(cv.addListener(ls))
    ls.consumeOrFail(Add(cv, Waiting))
  }

  it should "be Done(selected item) when the box has something selected" in {
    val cb = inSwing(newComboBox("a", "b", "c"))
    inSwing(cb.setSelectedIndex(1))
    val cv = inSwing(Binding.fromJComboBox(cb))
    val ls = inSwing(new LoggingListener[String])
    inSwing(cv.addListener(ls))
    ls.consumeOrFail(Add(cv, Done("b")))
  }

  "setSelectedIndex" should "perform the required changes" in {
    val cb = inSwing(newComboBox("a", "b", "c"))
    val cv = inSwing(Binding.fromJComboBox(cb))
    val ls = inSwing(new LoggingListener[String])
    inSwing(cv.addListener(ls))
    ls.consumeOrFail(Add(cv, Done("a")))
    inSwing(cb.setSelectedIndex(-1))
    ls.consumeOrFail(Change(cv, Done("a"), Waiting))
    inSwing(cb.setSelectedIndex(1))
    ls.consumeOrFail(Change(cv, Waiting, Done("b")))
    inSwing(cb.setSelectedIndex(2))
    ls.consumeOrFail(Change(cv, Done("b"), Done("c")))
  }

  "setSelectedItem" should "perform the required changes" in {
    val cb = inSwing(newComboBox("a", "b", "c"))
    val cv = inSwing(Binding.fromJComboBox(cb))
    val ls = inSwing(new LoggingListener[String])

    inSwing(cv.addListener(ls))
    ls.consumeOrFail(Add(cv, Done("a")))
    inSwing(cb.setSelectedItem(null))
    ls.consumeOrFail(Change(cv, Done("a"), Waiting))
    inSwing(cb.setSelectedItem("b"))
    ls.consumeOrFail(Change(cv, Waiting, Done("b")))
    inSwing(cb.setSelectedItem("c"))
    ls.consumeOrFail(Change(cv, Done("b"), Done("c")))
  }

  "selectWithKeyChar" should "perform the required changes" in {
    val cb = inSwing(newComboBox("a", "b", "c"))
    val cv = inSwing(Binding.fromJComboBox(cb))
    val ls = inSwing(new LoggingListener[String])

    inSwing(cv.addListener(ls))
    ls.consumeOrFail(Add(cv, Done("a")))
    inSwing(cb.setSelectedItem(null))                   // first, deselect to test the transition from deselected
    ls.consumeOrFail(Change(cv, Done("a"), Waiting))
    inSwing(cb.selectWithKeyChar('b'))
    ls.consumeOrFail(Change(cv, Waiting, Done("b")))
    inSwing(cb.selectWithKeyChar('c'))
    ls.consumeOrFail(Change(cv, Done("b"), Done("c")))
  }
}
