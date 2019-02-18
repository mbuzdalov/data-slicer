package ru.ifmo.ds.stat

/**
  * This is a class for results computed by statistical tests.
  *
  * @param statistic the value of the statistic that was computed on the measurements.
  * @param p the p-value, that is, the probability that the measurements,
  *          which are at least as extreme as the given ones according to the value of `statistic`,
  *          can happen assuming the null hypothesis is true.
  * @param test the statistical test that generated this result.
  * @param firstSampleSize the sample size of the first argument.
  * @param secondSampleSize the sample size of the second argument.
  * @tparam T the type for the statistic.
  */
case class TestResult[+T](statistic: T, p: Double, test: StatisticalTest[T],
                          firstSampleSize: Int, secondSampleSize: Int)
