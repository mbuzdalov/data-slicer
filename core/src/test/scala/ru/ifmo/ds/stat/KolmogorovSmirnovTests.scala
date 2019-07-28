package ru.ifmo.ds.stat

import java.util.Random

import scala.Ordering.Double.IeeeOrdering
import scala.annotation.tailrec

import org.scalatest.{FlatSpec, Matchers}

import ru.ifmo.ds.stat.util.KSUtils

class KolmogorovSmirnovTests extends FlatSpec with Matchers {
  private case class RefResult(p: Double, d: Double)

  private object R {
    // adapted from sources of R 3.4.1: src/library/stats/src/ks.c
    @tailrec
    final def pSmirnov2x(stat: Double, m: Int, n: Int): Double = {
      if (m > n) pSmirnov2x(stat, n, m) else {
        val r = Array.tabulate(n + 1)(j => j.toDouble / n)
        val u = Array.tabulate(n + 1)(j => if (r(j) >= stat) 0.0 else 1.0)

        for (i <- 1 to m) {
          val im = i.toDouble / m
          val w = i / (i.toDouble + n)
          u(0) = if (im >= stat) 0 else w * u(0)
          for (j <- 1 to n) {
            u(j) = if ((im - r(j)).abs >= stat) 0 else w * u(j) + u(j - 1)
          }
        }

        u(n)
      }
    }
  }

  private def checkRelativeMatch(expected: Double, found: Double, precision: Double): Unit = {
    found should be <= expected * (1 + precision)
    found should be >= expected * (1 - precision)
  }

  private def check(a: Seq[Int], b: Seq[Int], expected: RefResult): Unit = {
    val r1 = KolmogorovSmirnov.TwoSided(a, b)
    val r2 = KolmogorovSmirnov.TwoSided(b, a)
    checkRelativeMatch(expected.d, r1.statistic.toDouble, 1e-4)
    checkRelativeMatch(expected.p, r1.p, 2e-4)
    checkRelativeMatch(expected.d, r2.statistic.toDouble, 1e-4)
    checkRelativeMatch(expected.p, r2.p, 2e-4)
  }

  "The strict KS test" should "produce results equal to ones from R on inputs without duplicates" in {
    check(1 to 3, 4 to 6, RefResult(0.1, 1))
    check(1 to 4, 11 to 13, RefResult(0.05714, 1))
    check(1 to 5, 11 to 13, RefResult(0.03571, 1))
    check(1 to 4, 11 to 14, RefResult(0.02857, 1))
    check(1 to 10, 11 to 18, RefResult(4.571e-5, 1))
    check(1 to 11 by 2, 2 to 10 by 2, RefResult(1, 0.16667))
    check(1 to 11 by 2, 6 to 16 by 2, RefResult(0.474, 0.5))
  }

  "Two implementations of the two-sided KS probability computation" should "agree on randomly generated tests" in {
    val rng = new Random(7284368346522214L)

    def checkRandomBounded(bound: Int): Unit = {
      val n, m = 1 + rng.nextInt(bound)
      val p = if (rng.nextInt(10) == 0) 1 else rng.nextDouble()
      val v2x = R.pSmirnov2x(p, n, m)
      val v2y = KSUtils.pIdenticalDistributionIsInBounds(-p, p, n, m)
      v2y shouldBe (v2x +- 2e-15)
    }

    for (_ <- 0 until 100000) checkRandomBounded(20)
    for (_ <- 0 until 100) checkRandomBounded(200)
  }
}
