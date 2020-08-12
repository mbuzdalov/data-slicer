package com.github.mbuzdalov.swingasync.node

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.mbuzdalov.swingasync.node.Node._
import com.github.mbuzdalov.swingasync.Infrastructure._
import com.github.mbuzdalov.swingasync.LoggingListener
import com.github.mbuzdalov.swingasync.LoggingListener._

class NodeTests extends AnyFlatSpec with Matchers {
  private def happyHeavyWorkStory(n: Node, w: Workload, initState: State): Seq[LogRecord] = {
    IndexedSeq(                      // The story goes as follows:
      WorkBefore(w),                 // First the before-phase of the workload is executed.
      Change(n, initState, Running), // Then the workload is being scheduled and the change becomes visible.
      WorkMain(w),                   // Even if the main-phase completes before change is seen, we see it later.
      WorkAfter(w),                  // Next the after-phase is executed.
      Change(n, Running, Done)       // After that, the change from Running to Done becomes visible.
    )
  }

  private def happyLightWorkStory(n: Node, w: Workload, initState: State): Seq[LogRecord] = {
    IndexedSeq(                      // The story goes as follows:
      WorkBefore(w),                 // First the before-phase of the workload is executed, which does all the work.
      Change(n, initState, Running), // The Running state still appears, as otherwise any restart will not be visible.
      WorkAfter(w),                  // Next the after-phase is immediately notified of that.
      Change(n, Running, Done)       // After that, the change from Running to Done becomes visible.
    )
  }

  private def run(tag: String,
                  workloadGen: LoggingListener => Workload,
                  happyWorkStory: (Node, Workload, State) => Seq[LogRecord]): Unit = {
    s"A freshly created node without dependencies ($tag)" should "transit into Done as expected" in {
      // We run the test many times to be more confident that spurious failures don't happen.
      for (_ <- 0 to 10) {
        val listener = inSwing(new LoggingListener)
        val w = inSwing(workloadGen(listener))
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
        val w = inSwing(workloadGen(listener))
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

    s"Two dependent nodes ($tag)" should "initialize well when connecting, completing second, completing first" in {
      // We run the test many times to be more confident that spurious failures don't happen.
      for (_ <- 0 to 2) {
        val listener = inSwing(new LoggingListener)
        val w1, w2 = inSwing(workloadGen(listener))
        val n1 = inSwing(new MutableNode(w1))
        val n2 = inSwing(new MutableNode(w2))
        inSwing {
          n1.addListener(listener)
          n2.addListener(listener)
        }
        listener.consumeOrFail(Add(n1, Initializing), Add(n2, Initializing))
        inSwing(n1.addListener(n2))
        listener.failIfSomethingHappens(100)
        inSwing(n2.completeInitialization())
        listener.consumeOrFail(Change(n2, Initializing, Waiting))
        inSwing(n1.completeInitialization())
        listener.consumeOrFail(happyWorkStory(n1, w1, Initializing) :_*)
        listener.consumeOrFail(happyWorkStory(n2, w2, Waiting) :_*)
      }
    }

    they should "initialize well when connecting, completing first, completing second" in {
      // We run the test many times to be more confident that spurious failures don't happen.
      for (_ <- 0 to 2) {
        val listener = inSwing(new LoggingListener)
        val w1, w2 = inSwing(workloadGen(listener))
        val n1 = inSwing(new MutableNode(w1))
        val n2 = inSwing(new MutableNode(w2))
        inSwing {
          n1.addListener(listener)
          n2.addListener(listener)
        }
        listener.consumeOrFail(Add(n1, Initializing), Add(n2, Initializing))
        inSwing(n1.addListener(n2))
        listener.failIfSomethingHappens(100)
        inSwing(n1.completeInitialization())
        listener.consumeOrFail(happyWorkStory(n1, w1, Initializing) :_*)
        inSwing(n2.completeInitialization())
        listener.consumeOrFail(happyWorkStory(n2, w2, Initializing) :_*)
      }
    }

    they should "initialize well when completing then connecting" in {
      // We run the test many times to be more confident that spurious failures don't happen.
      for (_ <- 0 to 2) {
        val listener = inSwing(new LoggingListener)
        val w1, w2 = inSwing(workloadGen(listener))
        val n1 = inSwing(new MutableNode(w1))
        val n2 = inSwing(new MutableNode(w2))
        inSwing {
          n1.addListener(listener)
          n2.addListener(listener)
        }
        listener.consumeOrFail(Add(n1, Initializing), Add(n2, Initializing))
        inSwing(n1.completeInitialization())
        listener.consumeOrFail(happyWorkStory(n1, w1, Initializing) :_*)
        inSwing(n2.completeInitialization())
        listener.consumeOrFail(happyWorkStory(n2, w2, Initializing) :_*)
        inSwing(n1.addListener(n2))
        listener.consumeOrFail(happyWorkStory(n2, w2, Done) :_*)
      }
    }
  }

  run("heavy workload", l => new HeavyLoggingWorkload(l), happyHeavyWorkStory)
  run("light workload", l => new LightLoggingWorkload(l), happyLightWorkStory)
}
