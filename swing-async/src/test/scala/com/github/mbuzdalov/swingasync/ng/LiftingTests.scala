package com.github.mbuzdalov.swingasync.ng

import com.github.mbuzdalov.swingasync.ng.UpdatableValue._

class LiftingTests extends CommonTesting {
  private def checkSame[A](expected: UpdatableValue[A], found: UpdatableValue[A]): Unit = {
    found.state shouldEqual expected.state
  }

  "liftInSwing" should "compile and pass basic checks" in {
    val add = (a: Int, b: Int) => a + b
    val liftedFunction = Lifting.liftInSwing(add)

    checkSame(done(5), liftedFunction(done(2), done(3)))
    checkSame(waiting, liftedFunction(done(2), waiting))
    checkSame(waiting, liftedFunction(waiting, done(3)))
    checkSame(waiting, liftedFunction(waiting, waiting))

    val testException = new ArithmeticException("Test exception")
    checkSame(failed(new DependenciesFailedException(Some(testException))), liftedFunction(done(2), failed(testException)))
    checkSame(failed(new DependenciesFailedException(Some(testException))), liftedFunction(failed(testException), done(3)))
    checkSame(failed(new DependenciesFailedException(Some(testException))), liftedFunction(waiting, failed(testException)))
    checkSame(failed(new DependenciesFailedException(Some(testException))), liftedFunction(failed(testException), waiting))
    checkSame(failed(new DependenciesFailedException(Seq(testException, testException))), liftedFunction(failed(testException), failed(testException)))
  }
}
