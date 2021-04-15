package com.github.mbuzdalov.swingasync.ng

import shapeless._
import shapeless.ops.function.{FnFromProduct, FnToProduct}

import com.github.mbuzdalov.swingasync.ng.impl.InSwingFunctionalValue

object Lifting {
  def liftInSwing[F, LF <: HList, R,
                  G, LG <: HList](function: F)
                                 (implicit fun2gen: FnToProduct.Aux[F, LF => R],
                                           extractor: ShapelessHelpers.UpdatableValueDoneExtractor[LF, LG],
                                           gen2fun: FnFromProduct.Aux[LG => UpdatableValue[R], G]): G = {
    val unwrappedFunction = fun2gen(function)
    def internalLift(wArgs: LG) = new InSwingFunctionalValue(wArgs, extractor, unwrappedFunction)
    gen2fun(internalLift)
  }

  def liftNoneToFail[A, B](function: A => Option[B], noneMessage: String): UpdatableValue[A] => UpdatableValue[B] = {
    a => new UpdatableValue[B] {
      private[this] var myState: UpdatableValue.State[B] = UpdatableValue.Waiting
      private def setNewState(newState: UpdatableValue.State[B]): Unit = {
        val oldState = myState
        myState = newState
        notifyListeners(oldState)
      }
      private val myListener = new UpdatableValue.Listener[A] {
        private def digestOtherState(state: UpdatableValue.State[A]): Unit = setNewState(state match {
          case UpdatableValue.Done(v) => function(v) match {
            case Some(w) => UpdatableValue.Done(w)
            case None => UpdatableValue.Failed(new NoneToFailException(noneMessage))
          }
          case s@UpdatableValue.Failed(_) => s
          case UpdatableValue.Initializing => UpdatableValue.Initializing
          case UpdatableValue.Waiting => UpdatableValue.Waiting
          case UpdatableValue.Running => UpdatableValue.Running
          case UpdatableValue.Restarting => UpdatableValue.Restarting
        })

        override def addedToValue(value: UpdatableValue[A]): Unit = digestOtherState(value.state)
        override def valueChanged(value: UpdatableValue[A], oldState: UpdatableValue.State[A]): Unit = digestOtherState(value.state)
        override def removedFromValue(value: UpdatableValue[A]): Unit = setNewState(UpdatableValue.Failed(new NoneToFailException(noneMessage)))
      }
      a.addListener(myListener)

      override def state: UpdatableValue.State[B] = myState
    }
  }

  private class NoneToFailException(message: String) extends IllegalStateException(message)
}
