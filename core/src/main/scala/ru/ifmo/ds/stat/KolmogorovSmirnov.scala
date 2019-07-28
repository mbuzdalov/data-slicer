package ru.ifmo.ds.stat

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
    KSUtils.collectStatisticsWithProbabilities(firstSampleSize, secondSampleSize, oneMinusP)
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
    val p = 1 - oneMinusP(statistic, first.size, second.size)
    TestResult(statistic = statistic,
               p = math.min(1, p),
               test = this,
               firstSampleSize = first.size,
               secondSampleSize = second.size)
  }

  private def oneMinusP(statistic: Rational, firstSampleSize: Int, secondSampleSize: Int): Double = {
    KSUtils.pSmirnovDoesNotExceed(KSUtils.TwoSided)(statistic, firstSampleSize, secondSampleSize)
  }
}
