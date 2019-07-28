package ru.ifmo.ds.stat.util

import scala.annotation.tailrec

import spire.math.Rational

object KSUtils {
  /**
    * Computes the Kolmogorov-Smirnov statistics from the given measurements.
    * The method returns a pair of values: the minimum observed difference in the ECDFs, and the maximum one.
    *
    * @param a the first set of measurements.
    * @param b the second set of measurements.
    * @tparam T the type for a measurement.
    * @return the minimum and the maximum observed differences in the ECDFs of the given measurements.
    */
  def computeStatistics[T : Ordering](a: Iterable[T], b: Iterable[T]): (Rational, Rational) = {
    val as = a.toIndexedSeq.sorted
    val bs = b.toIndexedSeq.sorted
    val asSize = as.size
    val bsSize = bs.size

    val ord = implicitly[Ordering[T]]

    def nextGreaterOrSize(a: IndexedSeq[T], v: T, from: Int): Int = {
      val idx = a.indexWhere(i => ord.compare(v, i) < 0, from)
      if (idx == -1) a.size else idx
    }

    @tailrec
    def go(ai: Int, bi: Int, minDifference: Rational, maxDifference: Rational): (Rational, Rational) = {
      val theDiff = Rational(ai, asSize) - Rational(bi, bsSize)
      val newMinDifference = minDifference.min(theDiff)
      val newMaxDifference = maxDifference.max(theDiff)

      if (asSize == ai || bsSize == bi) (newMinDifference, newMaxDifference) else {
        val av = as(ai)
        val bv = bs(bi)
        val cmp = ord.compare(av, bv)
        if (cmp < 0) {
          go(nextGreaterOrSize(as, av, ai), bi, newMinDifference, newMaxDifference)
        } else if (cmp > 0) {
          go(ai, nextGreaterOrSize(bs, bv, bi), newMinDifference, newMaxDifference)
        } else {
          go(nextGreaterOrSize(as, av, ai), nextGreaterOrSize(bs, bv, bi), newMinDifference, newMaxDifference)
        }
      }
    }

    go(0, 0, 0, 0)
  }

  /**
    * Collects all possible statistics and the probability of observing exactly them.
    * @param firstSampleSize the first sample size.
    * @param secondSampleSize the second sample size.
    * @param pSmirnov the function that yields the one-minus-p-value from the statistic and sample sizes.
    * @return the iterable that contains all (statistic, its probability) pairs.
    */
  def collectStatisticsWithProbabilities(firstSampleSize: Int,
                                         secondSampleSize: Int,
                                         pSmirnov: (Rational, Int, Int) => Double): Iterable[(Rational, Double)] = {
    val firstR = (0 to firstSampleSize).map(x => Rational(x, firstSampleSize))
    val secondR = (0 to secondSampleSize).map(x => Rational(x, secondSampleSize))
    val stats = (for (f <- firstR; s <- secondR) yield (f - s).abs).distinct.sorted
    val pValues = stats.map(s => 1 - pSmirnov(s, firstSampleSize, secondSampleSize))
    val size = stats.size

    def diff(i: Int): Double = if (i + 1 == size) pValues(i) else pValues(i) - pValues(i + 1)

    stats.indices.map(i => stats(i) -> diff(i))
  }
}
