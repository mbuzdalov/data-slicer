package com.github.mbuzdalov.swingasync.ng

import javax.swing.{JCheckBox, JComboBox, JLabel}

import com.github.mbuzdalov.swingasync.ng.impl._

object Binding {
  def fromJCheckBox(checkBox: JCheckBox): UpdatableValue[Boolean] =
    new JCheckBoxBoundValue(checkBox)

  def fromJComboBox[T](comboBox: JComboBox[T]): UpdatableValue[T] =
    new JComboBoxBoundValue[T](comboBox)

  def forJLabel[T](printer: StatePrinter[T], label: JLabel): UpdatableValue.Listener[T] =
    new TextSettingListener[T](printer, label.setText, label.getText)
}
