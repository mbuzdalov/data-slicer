package ru.ifmo.ds.stat

import spire.math.Rational

import ru.ifmo.ds.stat.util.KSUtils

/**
  * The Kolmogorov-Smirnov test to be used while detecting differences
  */
object KolmogorovSmirnov {
  object TwoSided extends StatisticalTest[Rational] {
    override def name: String = "Two-sided Kolmogorov-Smirnov test"
    override def statisticValuesWithProbabilities(firstSampleSize: Int,
                                                  secondSampleSize: Int): Iterable[(Rational, Double)] = {
      KSUtils.collectStatisticsWithProbabilities(firstSampleSize, secondSampleSize, oneMinusP)
    }
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
      KSUtils.pSmirnovDoesNotExceed(-statistic, statistic, firstSampleSize, secondSampleSize)
    }
  }
}
