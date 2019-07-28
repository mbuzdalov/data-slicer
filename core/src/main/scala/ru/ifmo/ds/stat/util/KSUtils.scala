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
    * Compute the exact one-minus-p-value for the two-sided Kolmogorov-Smirnov test.
    *
    * The principle is as follows:
    *
    * `u(i, j)` is the probability of seeing `i` measurements on the left and `j` measurements on the right
    * at some moment of time when scanning the measurements in an increasing order, assuming:
    * - the distributions are identical;
    * - `m` measurements on the left in total;
    * - `n` measurements on the right in total;
    * - the response is continuous.
    *
    * Under the same assumptions, the next measurement we see is coming:
    * - from the left, probability `(m - i) / (m - i + n - j)`;
    * - from the right, probability `(n - j) / (m - i + n - j)`;
    * and we update `u(i + 1, j)` and `u(i, j + 1)` accordingly.
    *
    * Once we have an upper limit on the statistic, we zero out the `u(i, j)` which exceed this statistic or equal to it,
    * that is, we compute only the values which have `minStat < i / m - j / n < maxStat`.
    * Once we do that, `u(m, n)` will report the desired value.
    *
    * To reduce the computational burden, we scan `u(i, j)` in the order of increasing `i`, which enables
    * to have all computations done in `O(n)` space. As the updates come left-to-right, we can also
    * do everything not in two arrays, but in a single array.
    *
    * @param minStat the strict lower bound on a difference between the ECDFs of the two measurements.
    * @param maxStat the strict upper bound on a difference between the ECDFs of the two measurements.
    * @param m the sample size of the left-hand-side measurement.
    * @param n the sample size of the right-hand-side measurement.
    * @return the probability that the measurements do not exceed the statistic,
    *         assuming both left-hand-side and right-hand-side random variables have identical distributions.
    */
  @tailrec
  final def pIdenticalDistributionIsInBounds(minStat: Rational, maxStat: Rational, m: Int, n: Int): Double = {
    // I went an easy-to-get way, but actually, "minStat,  maxStat" is still OK here
    if (m > n) pIdenticalDistributionIsInBounds(-maxStat, -minStat, n, m) else {
      val u = Array.ofDim[Double](n + 1)

      u(0) = 1.0
      for (i <- 0 to m) {
        // minStat < i / m - j / n < maxStat; this produces the following bounds:
        //    j / n < i / m - minStat   =>   j < (i / m - minStat) * n
        //    i / m - maxStat < j / n   =>   (i / m - maxStat) * n < j
        val im = Rational(i, m)
        val jMinRaw = (im - maxStat) * n
        val jMaxRaw = (im - minStat) * n
        val jMin = math.max(0, if (jMinRaw.isWhole) jMinRaw.toInt + 1 else jMinRaw.ceil.toInt)
        val jMax = math.min(n, if (jMaxRaw.isWhole) jMaxRaw.toInt - 1 else jMaxRaw.floor.toInt)

        if (m > i) {
          val mi = (m - i).toDouble
          var j = jMin
          while (j < jMax) {
            val movPart = u(j) * (n - j) / (n - j + mi)
            u(j) -= movPart
            j += 1
            u(j) += movPart
          }
          u(j) = u(j) * mi / (n - j + mi)
        } else {
          var j = jMin
          while (j < jMax) {
            u(j + 1) += u(j)
            j += 1
          }
        }
      }
      u(n)
    }
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
                                         statExtractor: Rational => Rational,
                                         pSmirnov: (Rational, Int, Int) => Double): Iterable[(Rational, Double)] = {
    val firstR = (0 to firstSampleSize).map(x => Rational(x, firstSampleSize))
    val secondR = (0 to secondSampleSize).map(x => Rational(x, secondSampleSize))
    val stats = (for (f <- firstR; s <- secondR) yield statExtractor(f - s)).distinct.sorted
    val pValues = stats.map(s => 1 - pSmirnov(s, firstSampleSize, secondSampleSize))
    val size = stats.size

    def diff(i: Int): Double = if (i + 1 == size) pValues(i) else pValues(i) - pValues(i + 1)

    stats.indices.map(i => stats(i) -> diff(i))
  }
}
