package ru.ifmo.ds.gui.async

import java.util.concurrent.atomic.AtomicReference
import javax.swing.SwingUtilities

import org.scalatest.{FlatSpec, Matchers}

import ru.ifmo.ds.gui.async.LoggingListener._
import ru.ifmo.ds.gui.async.Node._

class NodeTests extends FlatSpec with Matchers {
  import NodeTests._

  private def happyWorkStory(n: Node, w: Workload, initState: State): Seq[LogRecord] = {
    IndexedSeq(                      // The story goes as follows:
      WorkBefore(w),                 // First the before-phase of the workload is executed.
      Change(n, initState, Running), // Then the workload is being scheduled and the change becomes visible.
      WorkMain(w),                   // Even if the main-phase completes before change is seen, we see it later.
      WorkAfter(w),                  // Next the after-phase is executed.
      Change(n, Running, Done)       // After that, the change from Running to Done becomes visible.
    )
  }

  "A freshly created node without dependencies" should "transit into Done as expected" in {
    // We run the test many times to be more confident that spurious failures don't happen.
    for (_ <- 0 to 10) {
      val listener = inSwing(new LoggingListener)
      val w = inSwing(new LoggingWorkload(listener))
      val n = inSwing(new MutableNode(w))
      inSwing(n.addListener(listener))
      listener.consumeOrFail(Add(n, Initializing))
      inSwing(n.completeInitialization())
      listener.consumeOrFail(happyWorkStory(n, w, Initializing) :_*)
      inSwing(n.removeListener(listener))
      listener.consumeOrFail(Remove(n, Done))
    }
  }

  it should "also restart well" in {
    // We run the test many times to be more confident that spurious failures don't happen.
    for (_ <- 0 to 10) {
      val listener = inSwing(new LoggingListener)
      val w = inSwing(new LoggingWorkload(listener))
      val n = inSwing(new MutableNode(w))
      inSwing(n.addListener(listener))
      listener.consumeOrFail(Add(n, Initializing))
      inSwing(n.completeInitialization())
      listener.consumeOrFail(happyWorkStory(n, w, Initializing) :_*)
      inSwing(n.restartEvaluation())
      listener.consumeOrFail(happyWorkStory(n, w, Done) :_*)
      inSwing(n.removeListener(listener))
      listener.consumeOrFail(Remove(n, Done))
    }
  }

  "Two dependent nodes" should "initialize well when connecting, completing second, completing first" in {
    // We run the test many times to be more confident that spurious failures don't happen.
    for (_ <- 0 to 2) {
      val listener = inSwing(new LoggingListener)
      val w1 = inSwing(new LoggingWorkload(listener))
      val w2 = inSwing(new LoggingWorkload(listener))
      val n1 = inSwing(new MutableNode(w1))
      val n2 = inSwing(new MutableNode(w2))
      inSwing {
        n1.addListener(listener)
        n2.addListener(listener)
      }
      listener.consumeOrFail(Add(n1, Initializing), Add(n2, Initializing))
      inSwing(n1.addListener(n2))
      listener.failIfSomethingHappens()
      inSwing(n2.completeInitialization())
      listener.consumeOrFail(Change(n2, Initializing, Waiting))
      inSwing(n1.completeInitialization())
      listener.consumeOrFail(
        WorkBefore(w1),                    // First, the workload of `n1` is started.
        Change(n1, Initializing, Running), // Next, the state change of `n1` becomes visible.
        WorkMain(w1),                      // The workload of `n1` continues...
        WorkAfter(w1),                     // ... then finishes...
        Change(n1, Running, Done),         // ... then the change becomes visible.
        WorkBefore(w2),                    // `n2` sees it and initiates its own workload.
        Change(n2, Waiting, Running),      // After work-before the change becomes visible.
        WorkMain(w2),                      // The workload of `n2` continues...
        WorkAfter(w2),                     // ... then finishes...
        Change(n2, Running, Done)          // ... then the change becomes visible.
      )
    }
  }
}

object NodeTests {
  private class Evaluation[T](fun: => T) extends Runnable {
    val value = new AtomicReference[T]()
    override def run(): Unit = {
      value.set(fun)
    }
  }

  private def inSwing[T](fun: => T): T = {
    val callable = new Evaluation(fun)
    SwingUtilities.invokeAndWait(callable)
    callable.value.get()
  }
}
