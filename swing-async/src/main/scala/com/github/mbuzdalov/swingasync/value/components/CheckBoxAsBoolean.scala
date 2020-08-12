package com.github.mbuzdalov.swingasync.value.components

import javax.swing.JCheckBox
import com.github.mbuzdalov.swingasync.node.{Node, SwitchableNode}
import com.github.mbuzdalov.swingasync.value.SwingValue

/**
 * This is a [[SwingValue]] holding a [[Boolean]] that is controlled by a [[JCheckBox]].
 */
class CheckBoxAsBoolean extends SwingValue[Boolean] {
  private[this] val myCheckbox = new JCheckBox()
  private[this] val myNode = new SwitchableNode(Node.Done)
  private[this] var myValue = false

  myCheckbox.addChangeListener(_ => {
    if (myValue != myCheckbox.isSelected) {
      myNode.setState(Node.Waiting)
      myValue = !myValue
      myNode.setState(Node.Done)
    }
  })

  /**
   * Returns the [[JCheckBox]] that controls the value of this [[SwingValue]].
   * @return the [[JCheckBox]] that controls the value of this [[SwingValue]].
   */
  def component: JCheckBox = myCheckbox

  override def value: Boolean = myValue
  override protected[value] def node: Node = myNode
  override def close(): Unit = {}
}