package com.github.mbuzdalov.swingasync.node

/**
 * This is a trait that describes the computational processes inside [[MutableNode]]s.
 *
 * In general, it works as follows:
 *
 * 1. In the Swing thread, the `beforeMain` method is called,
 * which either does the easy computation immediately and returns a [[Workload.LightResult]],
 * or prepares the input for a heavy computation and returns it as a [[Workload.HeavyResult]].
 *
 * 2. If the result of the previous step is [[Workload.HeavyResult]], a new thread is to be created
 * to compute the result asynchronously outside Swing. The computation is done by the `main` method.
 *
 * 3. The result - either a [[Workload.LightResult]] from the first step, or the outcome of `main` from
 * the second step - is then passed to the `afterMain` method in order to do all the necessary arrangements,
 * again in the Swing thread.
 *
 * 4. If there is any error during any of the stages, the `onError` method is called with the corresponding
 * exception. This is also done in the Swing thread.
 */
trait Workload {
  /**
   * The type of the input for the heavy computation.
   */
  type MainInput
  /**
   * The type of the output of the computation.
   */
  type MainOutput

  /**
   * This method is called on the Swing event dispatch thread when there is any exception thrown during computation.
   * @param th the exception that was thrown.
   */
  def onError(th: Throwable): Unit

  /**
   * This method is called on the Swing event dispatch thread to prepare the computation.
   *
   * If the whole computation is cheap enough to do immediately,
   * this result should be returned as [[Workload.LightResult]],
   * in this case no additional threads will be created, and this result will be directly shipped into `afterMain`.
   *
   * Otherwise, the result should be returned as [[Workload.HeavyResult]],
   * which will be then submitted to `main` into a dedicated thread.
   *
   * @return the result of computation preparation.
   */
  def beforeMain(): Workload.PreparationResult[MainInput, MainOutput]

  /**
   * This method is called to perform the heavy part of the computation outside the Swing event dispatch thread.
   * @param input the value prepared by `beforeMain` as an input to the heavy computation.
   * @return the result of the computation ready to be submitted to `afterMain`.
   */
  def main(input: MainInput): MainOutput

  /**
   * This method is called on the Swing event dispatch thread when the computation is done,
   * and the only remaining work is to update the contents exclusively accessible from within Swing.
   * @param input either the contents of the [[Workload.LightResult]] computed by `beforeMain`, or the result of `main`.
   */
  def afterMain(input: MainOutput): Unit
}

object Workload {
  /**
   * The trait that carries the result of computation preparation,
   * which can be light (that is, when the computation was easy and is already computed)
   * or heavy (which requires additional thread to be completely done).
   *
   * @tparam HeavyType the type of the input to the heavy computation.
   * @tparam LightType the type of the already-computed light computation.
   */
  sealed trait PreparationResult[+HeavyType, +LightType]

  /**
   * The implementation of [[PreparationResult]] that encodes the input to the heavy computation.
   * @param value the input to the heavy computation.
   * @tparam T the type of the input to the heavy computation.
   */
  case class HeavyResult[+T](value: T) extends PreparationResult[T, Nothing]

  /**
   * The implementation of [[PreparationResult]] that encodes the result of the light computation.
   * @param value the result of the light computation.
   * @tparam T the type of the result of the light computation.
   */
  case class LightResult[+T](value: T) extends PreparationResult[Nothing, T]
}
