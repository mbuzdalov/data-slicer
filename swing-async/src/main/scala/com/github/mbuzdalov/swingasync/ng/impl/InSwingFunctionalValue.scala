package com.github.mbuzdalov.swingasync.ng.impl

import shapeless.HList
import com.github.mbuzdalov.swingasync.ng.ShapelessHelpers.{UpdatableValueDoneExtractor => Extractor}
import com.github.mbuzdalov.swingasync.ng.UpdatableValue

import scala.collection.mutable.ArrayBuffer

class InSwingFunctionalValue[U <: HList, W <: HList, +R](args: W, extractor: Extractor[U, W], function: U => R)
  extends UpdatableValue[R]
{
  import UpdatableValue._

  private[this] val argCount: Int = args.runtimeLength
  private[this] var liveArgCount = 0
  private[this] var deadArgCount = 0
  private[this] var myState: State[R] = Waiting
  private[this] val myExceptions = new ArrayBuffer[Throwable]()
  private[this] val myListener = new Listener[Any] {
    override def addedToValue(value: UpdatableValue[Any]): Unit =
      updateCount(value.state, isAdd = true)
    override def removedFromValue(value: UpdatableValue[Any]): Unit =
      updateCount(value.state, isAdd = false)
    override def valueChanged(value: UpdatableValue[Any], oldState: State[Any]): Unit = {
      updateCount(oldState, isAdd = false)
      updateCount(value.state, isAdd = true)
    }
  }

  extractor.addListener(args, myListener)

  override def state: State[R] = myState

  private[this] def updateCount(state: State[Any], isAdd: Boolean): Unit = state match {
    case Done(_) => if (isAdd) {
      assert(deadArgCount + liveArgCount < argCount)
      liveArgCount += 1
      if (liveArgCount == argCount) {
        updateState(try {
          Done(function(extractor.unwrap(args)))
        } catch {
          case th: Throwable => Failed(th)
        })
      }
    } else {
      assert(liveArgCount > 0)
      if (liveArgCount == argCount) updateState(Waiting)
      liveArgCount -= 1
    }

    case Failed(th) => if (isAdd) {
      assert(deadArgCount + liveArgCount < argCount)
      assert(deadArgCount == myExceptions.size)
      deadArgCount += 1
      myExceptions.addOne(th)
      updateState(Failed(combineExceptions()))
    } else {
      assert(deadArgCount > 0)
      assert(deadArgCount == myExceptions.size)
      val index = myExceptions.indexOf(th)
      assert(index >= 0)
      myExceptions.remove(index)
      deadArgCount -= 1
      updateState(if (deadArgCount == 0) Waiting else Failed(combineExceptions()))
    }

    case _ =>
  }

  private[this] def combineExceptions(): Throwable = new DependenciesFailedException(myExceptions)

  private[this] def updateState(newState: State[R]): Unit = {
    val oldState = myState
    myState = newState
    notifyListeners(oldState)
  }
}
