package ru.ifmo.ds.gui.async

import scala.collection.mutable

import javax.swing.SwingUtilities

/**
  * This is a public interface to the async node.
  */
trait Node {
  private[this] val listeners = new mutable.HashSet[NodeListener]()

  /**
    * Adds the given listener to this node. If this listener is already added, nothing happens.
    * @param listener the listener to add.
    */
  def addListener(listener: NodeListener): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    if (!listeners.contains(listener)) {
      listeners += listener
      listener.nodeJustAdded(this, getState)
    }
  }

  /**
    * Removes the given listener from this node. If this listener is not added, nothing happens.
    * @param listener the listener to remove.
    */
  def removeListener(listener: NodeListener): Unit = {
    require(SwingUtilities.isEventDispatchThread)
    if (listeners.contains(listener)) {
      listener.nodeJustRemoved(this, getState)
      listeners -= listener
    }
  }

  /**
    * Returns the current state of the node.
    * @return the state.
    */
  protected def getState: Node.State

  /**
    * Notifies all the listeners that the state is changed.
    * @param oldState the old state of the node.
    */
  protected def notifyOfStateChange(oldState: Node.State): Unit = {
    val state = getState
    listeners.foreach(_.stateChanged(this, oldState, state))
  }
}

object Node {
  sealed abstract class State(val name: String)
  case object Waiting extends State("Waiting")
  case object Running extends State("Running")
  case object Restarting extends State("Restarting")
  case object Failed extends State("Failed")
  case object Done extends State("Done")
}
