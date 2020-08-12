package com.github.mbuzdalov.swingasync

import java.util.concurrent.atomic.AtomicReference

import javax.swing.SwingUtilities

object Infrastructure {
  private class Evaluation[T](fun: => T) extends Runnable {
    val value = new AtomicReference[T]()
    override def run(): Unit = {
      value.set(fun)
    }
  }

  def inSwing[T](fun: => T): T = {
    val callable = new Evaluation(fun)
    SwingUtilities.invokeAndWait(callable)
    callable.value.get()
  }
}
