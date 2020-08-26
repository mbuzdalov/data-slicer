package com.github.mbuzdalov.swingasync.ng

abstract class UpdatableValue[+A] {
  def state: UpdatableValue.State[A]
}

object UpdatableValue {
  sealed abstract class State[+A](val name: String)
  case object Initializing extends State[Nothing]("Initializing")
  case object Waiting extends State[Nothing]("Waiting")
  case object Running extends State[Nothing]("Running")
  case object Restarting extends State[Nothing]("Restarting")
  case class Failed(throwable: Throwable) extends State[Nothing]("Failed")
  case class Done[+A](value: A) extends State[A]("Done")
}
