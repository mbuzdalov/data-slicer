package ru.ifmo.ds.gui.async.value

import ru.ifmo.ds.gui.async.node.Node

class ConstantSwingValue[T](val value: T) extends SwingValue[T] {
  override protected[value] def node: Node = Node.DoneNode
  override def close(): Unit = {}
}
