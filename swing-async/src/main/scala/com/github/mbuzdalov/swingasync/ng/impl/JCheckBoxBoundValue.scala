package com.github.mbuzdalov.swingasync.ng.impl

import javax.swing.JCheckBox
import javax.swing.event.ChangeListener

import com.github.mbuzdalov.swingasync.ng.UpdatableValue

class JCheckBoxBoundValue(source: JCheckBox) extends UpdatableValue[Boolean] {
  import UpdatableValue._

  private[this] var myState: Done[Boolean] = Done(source.isSelected)
  private[this] val myListener: ChangeListener = _ => {
    val boxState = source.isSelected
    if (myState.value != boxState) {
      val oldState = myState
      myState = Done(boxState)
      notifyListeners(oldState)
    }
  }

  source.addChangeListener(myListener)
  override def state: State[Boolean] = myState
}
