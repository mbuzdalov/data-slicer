package com.github.mbuzdalov.swingasync.node

import com.github.mbuzdalov.swingasync.node.Node.State

/**
  * This is an interface for async node listeners.
  */
trait NodeListener {
  /**
    * This is called from inside the `AsyncNode.addListener` method when this listener is added.
    * @param node the node, to which the listener has just been added.
    * @param state the state of the node.
    */
  def nodeJustAdded(node: Node, state: State): Unit

  /**
    * This is called when the state of the node is changed.
    * @param node the node whose state is changed.
    * @param oldState the previous state of the node.
    * @param newState the new (current) state of the node.
    */
  def stateChanged(node: Node, oldState: State, newState: State): Unit

  /**
    * This is called from inside the `AsyncNode.removeListener` method when this listener is removed.
    * @param node the node, from which the listener is about to be removed.
    * @param state the state of the node.
    */
  def nodeJustRemoved(node: Node, state: State): Unit
}
