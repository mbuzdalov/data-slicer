package ru.ifmo.ds.stat

import scala.annotation.tailrec

import spire.math.Rational

import ru.ifmo.ds.stat.util.KSUtils

/**
  * The Kolmogorov-Smirnov test to be used while detecting differences
  */
object KolmogorovSmirnov extends StatisticalTest[Rational] {
  /**
    * Returns the name of the statistical test.
    * @return the name of the statistical test.
    */
  override def name: String = "Two-sided Kolmogorov-Smirnov test"

  /**
    * Returns an iterable over all possible statistic values with the given sample sizes,
    * along with their probabilities.
    *
    * The probability of a statistic to appear will generally be its corresponding
    * p-value minus the p-value of the next extreme statistic.
    *
    * @return statistics and their probabilities for the given sample sizes.
    */
  override def statisticValuesWithProbabilities(firstSampleSize: Int,
                                                secondSampleSize: Int): Iterable[(Rational, Double)] = {
    KSUtils.collectStatisticsWithProbabilities(firstSampleSize, secondSampleSize, pSmirnovDoesNotExceedTwoSided)
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
    * that is, we compute only the values which have `|i / m - j / n| < stat`.
    * Once we do that, `u(m, n)` will report the desired value.
    *
    * To reduce the computational burden, we scan `u(i, j)` in the order of increasing `i`, which enables
    * to have all computations done in `O(n)` space. As the updates come left-to-right, we can also
    * do everything not in two arrays, but in a single array.
    *
    * @param stat the strict upper bound on a difference between the two measurements (the Kolmogorov-Smirnov statistic).
    * @param m the sample size of the left-hand-side measurement.
    * @param n the sample size of the right-hand-side measurement.
    * @return the probability that the measurements do not exceed the statistic,
    *         assuming both left-hand-side and right-hand-side random variables have identical distributions.
    */
  @tailrec
  private[stat] final def pSmirnovDoesNotExceedTwoSided(stat: Rational, m: Int, n: Int): Double = {
    if (m > n) pSmirnovDoesNotExceedTwoSided(stat, n, m) else {
      val u = Array.ofDim[Double](n + 1)

      u(0) = 1.0
      for (i <- 0 to m) {
        val im = Rational(i, m)
        val jMinRaw = (im - stat) * n
        val jMaxRaw = (im + stat) * n
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
    * Applies the two-sided Kolmogorov-Smirnov test to the given measurements.
    * @param first the first set of measurements.
    * @param second the second set of measurements.
    * @tparam T the type of the elements in the sets.
    * @return the test result.
    */
  override def apply[T : Ordering](first: Iterable[T], second: Iterable[T]): TestResult[Rational] = {
    val (minDiff, maxDiff) = KSUtils.computeStatistics(first, second)
    val statistic = maxDiff.max(-minDiff)
    val p = 1 - pSmirnovDoesNotExceedTwoSided(statistic, first.size, second.size)
    TestResult(statistic = statistic,
               p = math.min(1, p),
               test = this,
               firstSampleSize = first.size,
               secondSampleSize = second.size)
  }
}
