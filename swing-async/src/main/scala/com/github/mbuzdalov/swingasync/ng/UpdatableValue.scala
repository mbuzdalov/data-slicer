package com.github.mbuzdalov.swingasync.ng

import java.util.{Arrays => JArrays}
import scala.collection.mutable.ArrayBuffer

abstract class UpdatableValue[+A] {
  private[this] val listeners = new ArrayBuffer[UpdatableValue.Listener[A]]()

  def state: UpdatableValue.State[A]

  def addListener(listener: UpdatableValue.Listener[A]): Unit = {
    listeners.addOne(listener)
    listener.addedToValue(this)
  }

  def removeListener(listener: UpdatableValue.Listener[A]): Unit = {
    val index = listeners.indexOf(listener)
    if (index >= 0) {
      listeners.remove(index)
      listener.removedFromValue(this)
    }
  }

  protected[this] def notifyListeners(oldState: UpdatableValue.State[A]): Unit =
    listeners.foreach(_.valueChanged(this, oldState))
}

object UpdatableValue {
  sealed abstract class State[+A](val name: String)
  case object Initializing extends State[Nothing]("Initializing")
  case object Waiting extends State[Nothing]("Waiting")
  case object Running extends State[Nothing]("Running")
  case object Restarting extends State[Nothing]("Restarting")
  case class Failed(throwable: Throwable) extends State[Nothing]("Failed")
  case class Done[+A](value: A) extends State[A]("Done")

  trait Listener[-A] {
    def addedToValue(value: UpdatableValue[A]): Unit
    def valueChanged(value: UpdatableValue[A], oldState: State[A]): Unit
    def removedFromValue(value: UpdatableValue[A]): Unit
  }

  class DependenciesFailedException(suppressedList: Iterable[Throwable])
    extends RuntimeException("One of the dependencies failed", null, true, false)
  {
    suppressedList.foreach(addDependency)

    private def addDependency(throwable: Throwable): Unit = throwable match {
      case e: DependenciesFailedException => e.getSuppressed.foreach(addDependency)
      case _ => addSuppressed(throwable)
    }

    private def getAnyRefSuppressed: Array[AnyRef] = getSuppressed.asInstanceOf[Array[AnyRef]]

    override def equals(obj: Any): Boolean = obj match {
      case that: DependenciesFailedException => JArrays.equals(getAnyRefSuppressed, that.getAnyRefSuppressed)
      case _ => false
    }

    override def hashCode(): Int = JArrays.deepHashCode(getAnyRefSuppressed)
  }
}
