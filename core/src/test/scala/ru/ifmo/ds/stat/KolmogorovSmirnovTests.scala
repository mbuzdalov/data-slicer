package ru.ifmo.ds.stat

import java.util.Random

import org.scalatest.{FlatSpec, Matchers}

class KolmogorovSmirnovTests extends FlatSpec with Matchers {
  private[this] def check(a: Seq[Int], b: Seq[Int], strict: Boolean, expected: KolmogorovSmirnov.Result): Unit = {
    val result = KolmogorovSmirnov(a, b, strict)
    result.d shouldBe (expected.d +- 1e-4)
    result.p shouldBe (expected.p +- 1e-4)
  }

  private[this] def checkStrict(a: Seq[Int], b: Seq[Int], expected: KolmogorovSmirnov.Result): Unit = {
    check(a, b, strict = true, expected)
  }

  private[this] def checkNonStrict(a: Seq[Int], b: Seq[Int], expected: KolmogorovSmirnov.Result): Unit = {
    check(a, b, strict = false, expected)
  }

  "The strict KS test" should "produce results equal to ones from R on inputs without duplicates" in {
    checkStrict(1 to 3, 4 to 6, KolmogorovSmirnov.Result(0.1, 1))
    checkStrict(1 to 4, 11 to 13, KolmogorovSmirnov.Result(0.05714, 1))
    checkStrict(1 to 5, 11 to 13, KolmogorovSmirnov.Result(0.03571, 1))
    checkStrict(1 to 4, 11 to 14, KolmogorovSmirnov.Result(0.02857, 1))
    checkStrict(1 to 10, 11 to 18, KolmogorovSmirnov.Result(4.571e-5, 1))
    checkStrict(1 to 11 by 2, 2 to 10 by 2, KolmogorovSmirnov.Result(1, 0.16667))
    checkStrict(1 to 11 by 2, 6 to 16 by 2, KolmogorovSmirnov.Result(0.474, 0.5))
  }

  "The non-strict KS test" should "produce results equal to ones from R on inputs without duplicates" in {
    checkNonStrict(1 to 3, 4 to 6, KolmogorovSmirnov.Result(0.09956, 1))
    checkNonStrict(1 to 4, 11 to 13, KolmogorovSmirnov.Result(0.06486, 1))
    checkNonStrict(1 to 5, 11 to 13, KolmogorovSmirnov.Result(0.04703, 1))
    checkNonStrict(1 to 4, 11 to 14, KolmogorovSmirnov.Result(0.03663, 1))
    checkNonStrict(1 to 10, 11 to 18, KolmogorovSmirnov.Result(0.0002758, 1))
    checkNonStrict(1 to 11 by 2, 2 to 10 by 2, KolmogorovSmirnov.Result(1, 0.16667))
    checkNonStrict(1 to 11 by 2, 6 to 16 by 2, KolmogorovSmirnov.Result(0.4413, 0.5))
  }

  "Two implementations of pSmirnov2x" should "agree on randomly generated tests" in {
    val rng = new Random(7284368346522214L)

    def checkRandomBounded(bound: Int) = {
      val n, m = 1 + rng.nextInt(bound)
      val p = rng.nextDouble()
      val v2x = KolmogorovSmirnov.pSmirnov2x(p, n, m)
      val v2y = KolmogorovSmirnov.pSmirnov2y(p, n, m)
      v2y shouldBe (v2x +- 1e-9)
    }

    for (_ <- 0 until 100000) checkRandomBounded(20)
    for (_ <- 0 until 100) checkRandomBounded(200)
  }
}
