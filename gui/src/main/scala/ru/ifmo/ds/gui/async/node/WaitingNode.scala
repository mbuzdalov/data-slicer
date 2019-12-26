package ru.ifmo.ds.gui.async.node

/**
 * This is a node which is always waiting.
 *
 * If one need to add many dependencies and avoid lengthy computations while doing that,
 * one can add this node to the dependencies first, do all the dirty job, and remove this node from the dependencies.
 */
object WaitingNode extends Node {
  override protected def getState: Node.State = Node.Waiting
}
