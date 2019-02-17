package ru.ifmo.ds.stat

/**
  * This is a class for results computed by statistical tests.
  * @tparam T the type for the statistic.
  */
case class TestResult[+T](/**
                            * Returns the value of the statistic that was computed on the measurements.
                            * @return the value of the statistic.
                            */
                          statistic: T,
                          /**
                            * Returns the p-value, that is, the probability that the measurements,
                            * which are at least as extreme as the given ones according to the value of `statistic`,
                            * can happen assuming the null hypothesis is true.
                            * @return the p-value.
                            */
                          p: Double,
                          /**
                            * Returns the statistical test that generated this result.
                            * @return the statistical test.
                            */
                          test: StatisticalTest[T],
                          /**
                            * Returns the sample size of the first argument.
                            * @return the sample size of the first argument.
                            */
                          firstSampleSize: Int,
                          /**
                            * Returns the sample size of the second argument.
                            * @return the sample size of the second argument.
                            */
                          secondSampleSize: Int)
