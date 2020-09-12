package com.github.mbuzdalov.swingasync.ng

import com.github.mbuzdalov.swingasync.ng.UpdatableValue._

class LiftingTests extends CommonTesting {
  private def checkSame[A](expected: UpdatableValue[A], found: UpdatableValue[A]): Unit = {
    found.state shouldEqual expected.state
  }

  "liftInSwing" should "compile and pass basic checks" in {
    val add = (a: Int, b: Int) => a + b
    val lfAdd = Lifting.liftInSwing(add)

    checkSame(done(5), lfAdd(done(2), done(3)))
    checkSame(waiting, lfAdd(done(2), waiting))
    checkSame(waiting, lfAdd(waiting, done(3)))
    checkSame(waiting, lfAdd(waiting, waiting))

    val testEx = new ArithmeticException("Test exception")
    checkSame(failed(new DependenciesFailedException(Some(testEx))), lfAdd(done(2), failed(testEx)))
    checkSame(failed(new DependenciesFailedException(Some(testEx))), lfAdd(failed(testEx), done(3)))
    checkSame(failed(new DependenciesFailedException(Some(testEx))), lfAdd(waiting, failed(testEx)))
    checkSame(failed(new DependenciesFailedException(Some(testEx))), lfAdd(failed(testEx), waiting))
    checkSame(failed(new DependenciesFailedException(Seq(testEx, testEx))), lfAdd(failed(testEx), failed(testEx)))
  }
}
