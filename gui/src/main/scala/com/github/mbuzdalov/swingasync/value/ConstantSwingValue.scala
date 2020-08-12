package com.github.mbuzdalov.swingasync.value

import com.github.mbuzdalov.swingasync.node.Node

class ConstantSwingValue[+T](val value: T) extends SwingValue[T] {
  override protected[value] def node: Node = Node.DoneNode
  override def close(): Unit = {}
}
