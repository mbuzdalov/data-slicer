package ru.ifmo.ds.stat

trait StatisticalTest[+Statistic] {
  /**
    * Returns the name of the statistical test.
    * @return the name of the statistical test.
    */
  def name: String

  /**
    * Applies the statistical test to the given measurements.
    * @param first the first set of measurements.
    * @param second the second set of measurements.
    * @tparam T the type of the elements in the sets.
    * @return the test result.
    */
  def apply[T : Ordering](first: Iterable[T], second: Iterable[T]): TestResult[Statistic]

  /**
    * Returns an iterable over all possible statistic values with the given sample sizes,
    * along with their probabilities.
    *
    * The probability of a statistic to appear will generally be its corresponding
    * p-value minus the p-value of the next extreme statistic.
    *
    * @return statistics and their probabilities for the given sample sizes.
    */
  def statisticValuesWithProbabilities(firstSampleSize: Int, secondSampleSize: Int): Iterable[(Statistic, Double)]
}
