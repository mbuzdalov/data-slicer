package ru.ifmo.ds.gui.async.value

import ru.ifmo.ds.gui.async.node.Node

/**
 * This is an abstract class for every value which is associated with some Swing entity, maybe intermediate,
 * which can depend on other entities and can be recomputed if these dependencies change.
 *
 * @tparam T the type of the value.
 */
abstract class SwingValue[+T] extends AutoCloseable {
  /**
   * Returns the current value.
   * Can be called only when [[isReady]] returns `true`, otherwise exceptions are thrown.
   *
   * @return the current value.
   */
  def value: T

  /**
   * Returns whether the value is ready.
   * @return `true` if the value is ready, `false` otherwise.
   */
  def isReady: Boolean = node.getState == Node.Done

  /**
   * This is the [[Node]] for asynchronous computations, which is associated with the current value.
   * All the dependencies are actually managed by adding and removing listeners to this node,
   * but the user of the value cannot access this node directly.
   */
  protected[value] def node: Node
}

object SwingValue {
  /**
    * Creates a [[SwingValue]] which holds the given constant value and does not depend on anything.
    * @param value the value for the [[SwingValue]]to always have.
    * @tparam T the type of the value.
    * @return the [[SwingValue]] which holds `value` and never changes.
    */
  def constant[T](value: T): SwingValue[T] = new ConstantSwingValue(value)

  /**
    * Binds the given one-argument function to the [[SwingValue]] having the type matching the first argument.
    * @param arg the [[SwingValue]] to bind the first argument to.
    * @param function the function to bind.
    * @tparam A the type of the argument.
    * @tparam R the type of the result.
    * @return the [[SwingValue]] which is connected to `arg` through `function`.
    */
  def bind[A, R](arg: SwingValue[A])(function: A => R): SwingValue[R] =
    new BoundSwingValue(arg.value, function, arg.node)

  /**
    * Binds the given two-argument function to the [[SwingValue]]s having the types matching the arguments.
    * @param arg1 the [[SwingValue]] to bind the first argument to.
    * @param arg2 the [[SwingValue]] to bind the second argument to.
    * @param function the function to bind.
    * @tparam A1 the type of the first argument.
    * @tparam A2 the type of the second argument.
    * @tparam R the type of the result.
    * @return the [[SwingValue]] which is connected to `arg1` and `arg2` through `function`.
    */
  def bind[A1, A2, R](arg1: SwingValue[A1], arg2: SwingValue[A2])
                     (function: (A1, A2) => R): SwingValue[R] =
    new BoundSwingValue((arg1.value, arg2.value), function.tupled,
                        arg1.node, arg2.node)

  /**
    * Binds the given three-argument function to the [[SwingValue]]s having the types matching the arguments.
    * @param arg1 the [[SwingValue]] to bind the first argument to.
    * @param arg2 the [[SwingValue]] to bind the second argument to.
    * @param arg3 the [[SwingValue]] to bind the third argument to.
    * @param function the function to bind.
    * @tparam A1 the type of the first argument.
    * @tparam A2 the type of the second argument.
    * @tparam A3 the type of the second argument.
    * @tparam R the type of the result.
    * @return the [[SwingValue]] which is connected to `arg1`, `arg2` and `arg3` through `function`.
    */
  def bind[A1, A2, A3, R](arg1: SwingValue[A1], arg2: SwingValue[A2], arg3: SwingValue[A3])
                         (function: (A1, A2, A3) => R): SwingValue[R] =
    new BoundSwingValue((arg1.value, arg2.value, arg3.value), function.tupled,
                        arg1.node, arg2.node, arg3.node)

  /**
    * Binds the given function from a sequence of `A`s to a sequence of [[SwingValue]]s of type `A`.
    * @param function the function to bind.
    * @param args the [[SwingValue]]s to bind the argument to.
    * @tparam A the type of the element of the sequence which is the first argument.
    * @tparam R the type of the result.
    * @return the [[SwingValue]] which is connected to `args` through `function`.
    */
  def bind[A, R](args: SwingValue[A]*)(function: Seq[A] => R): SwingValue[R] =
    new BoundSwingValue[Seq[A], R](args.map(_.value), function, args.map(_.node) :_*)
}
