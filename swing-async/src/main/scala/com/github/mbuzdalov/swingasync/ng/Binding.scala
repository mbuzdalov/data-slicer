package com.github.mbuzdalov.swingasync.ng

import javax.swing.JCheckBox

import com.github.mbuzdalov.swingasync.ng.impl._

object Binding {
  def fromJCheckBox(checkBox: JCheckBox): UpdatableValue[Boolean] = new JCheckBoxBoundValue(checkBox)
}
