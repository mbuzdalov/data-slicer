package com.github.mbuzdalov.swingasync.ng.impl

import com.github.mbuzdalov.swingasync.ng.{StatePrinter, UpdatableValue}

class TextSettingListener[-T](printer: StatePrinter[T], setFunction: String => Unit,
                              setBackValue: String) extends UpdatableValue.Listener[T] {
  private def setLabelText(state: UpdatableValue.State[T]): Unit = setFunction(printer.toString(state))

  override def addedToValue(value: UpdatableValue[T]): Unit = setLabelText(value.state)
  override def valueChanged(value: UpdatableValue[T], oldState: UpdatableValue.State[T]): Unit = setLabelText(value.state)
  override def removedFromValue(value: UpdatableValue[T]): Unit = setFunction(setBackValue)
}
