package ru.ifmo.ds.gui.async.util

import ru.ifmo.ds.gui.async.Node

/**
  * This is a node which is always waiting.
  *
  * If one need to add many dependencies and avoid lengthy recomputing while doing that,
  * one can add this node to the dependencies first, do all the dirty job, and remove this node from the dependencies.
  */
object AlwaysWaitingNode extends Node {
  override protected def getState: Node.State = Node.Waiting
}
