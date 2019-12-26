package ru.ifmo.ds.gui.async.value

import ru.ifmo.ds.gui.async.node.Node

/**
 * This is an abstract class for every value which is associated with some Swing entity, maybe intermediate,
 * which can depend on other entities and can be recomputed if these dependencies change.
 *
 * @tparam T the type of the value.
 */
abstract class SwingValue[T] {
  /**
   * Returns the current value.
   * @return the current value.
   */
  def value: T

  /**
   * This is the `Node` for asynchronous computations, which is associated with the current value.
   * All the dependencies are actually managed by adding and removing listeners to this node,
   * but the user of the value cannot access this node directly.
   */
  protected val node: Node
}

object SwingValue {
  def bind[A, R](function: A => R)(arg: SwingValue[A]): SwingValue[R] = ???
  def bind[A1, A2, R](function: (A1, A2) => R)(arg1: SwingValue[A1], arg2: SwingValue[A2]): SwingValue[R] = ???
}
