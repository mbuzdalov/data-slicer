package com.github.mbuzdalov.swingasync.ng.impl

import java.awt.event.ActionListener

import javax.swing.JComboBox

import com.github.mbuzdalov.swingasync.ng.UpdatableValue

class JComboBoxBoundValue[T](source: JComboBox[T]) extends UpdatableValue[T] {
  import UpdatableValue._

  private[this] def getSourceState: State[T] = {
    val index = source.getSelectedIndex
    if (index < 0) Waiting else Done(source.getItemAt(index))
  }
  private[this] var myState: State[T] = getSourceState
  private[this] val myListener: ActionListener = _ => {
    val newState = getSourceState
    if (newState != myState) {
      val oldState = myState
      myState = newState
      notifyListeners(oldState)
    }
  }

  source.addActionListener(myListener)
  override def state: UpdatableValue.State[T] = myState
}
