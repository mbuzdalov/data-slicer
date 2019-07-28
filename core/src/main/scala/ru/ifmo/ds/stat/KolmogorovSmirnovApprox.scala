package ru.ifmo.ds.stat

import scala.annotation.tailrec

import spire.math.Rational

import ru.ifmo.ds.stat.util.KSUtils

/**
  * The approximate version of the Kolmogorov-Smirnov test to be used while detecting differences
  */
object KolmogorovSmirnovApprox extends StatisticalTest[Rational] {
  private def approx(stat: Double): Double = {
    val atZero = math.exp(-2 * stat * stat)
    @tailrec
    def impl(soFar: Double, k: Double): Double = {
      val addend = 2 * math.pow(atZero, k * k) * (2 * (k % 2) - 1)
      if (addend == 0) soFar else impl(soFar + addend, k + 1)
    }
    impl(0, 1)
  }

  /**
    * Applies the two-sided approximate Kolmogorov-Smirnov test to the given measurements.
    * @param first the first set of measurements.
    * @param second the second set of measurements.
    * @tparam T the type of the elements in the sets.
    * @return the test result.
    */
  override def apply[T : Ordering](first: Iterable[T], second: Iterable[T]): TestResult[Rational] = {
    val asSize = first.size
    val bsSize = second.size
    val (minDiff, maxDiff) = KSUtils.computeStatistics(first, second)
    val statistic = maxDiff.max(-minDiff)
    val p = approx(statistic.toDouble * math.sqrt(asSize.toDouble * bsSize / (asSize + bsSize)))
    TestResult(statistic = statistic,
               p = math.min(1, p),
               test = this,
               firstSampleSize = asSize,
               secondSampleSize = bsSize)
  }

  /**
    * Returns the name of the statistical test.
    *
    * @return the name of the statistical test.
    */
  override def name: String = "Approximate two-sided Kolmogorov-Smirnov test"

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
    KSUtils.collectStatisticsWithProbabilities(firstSampleSize, secondSampleSize, (s, _, _) => approx(s.toDouble))
  }
}
