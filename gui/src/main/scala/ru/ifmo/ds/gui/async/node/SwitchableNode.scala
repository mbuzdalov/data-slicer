package ru.ifmo.ds.gui.async.node

import javax.swing.SwingUtilities

/**
  * This is the node, whose state can be programmatically controlled from the outside.
  * @param initialState the initial state of this node.
  */
class SwitchableNode(initialState: Node.State) extends Node {
  private[this] var state = initialState

  override def getState: Node.State = state

  /**
    * Sets the state of this node to the given one.
    *
    * If this is called on the Swing event dispatch thread, the change will happen instantly.
    * Otherwise, `SwingUtilities.invokeLater` is used.
    *
    * @param newState the new state to be set.
    */
  def setState(newState: Node.State): Unit = {
    require(newState != null)
    if (newState != state) {
      if (SwingUtilities.isEventDispatchThread) {
        val oldState = state
        state = newState
        notifyOfStateChange(oldState)
      } else {
        SwingUtilities.invokeLater(() => setState(newState))
      }
    }
  }
}
