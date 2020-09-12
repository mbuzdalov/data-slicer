package com.github.mbuzdalov.swingasync.ng

import javax.swing.{JCheckBox, JComboBox}

import com.github.mbuzdalov.swingasync.ng.impl._

object Binding {
  def fromJCheckBox(checkBox: JCheckBox): UpdatableValue[Boolean] = new JCheckBoxBoundValue(checkBox)
  def fromJComboBox[T](comboBox: JComboBox[T]): UpdatableValue[T] = new JComboBoxBoundValue[T](comboBox)
}
