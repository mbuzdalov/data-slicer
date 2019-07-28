package ru.ifmo.ds.stat

import spire.math.Rational

import ru.ifmo.ds.stat.util.KSUtils

/**
  * The Kolmogorov-Smirnov test to be used while detecting differences
  */
object KolmogorovSmirnov {
  private trait KSCommon extends StatisticalTest[Rational] {
    protected def diff2stat(diff: Rational): Rational
    protected def pair2stat(minDiff: Rational, maxDiff: Rational): Rational
    protected def stat2pair(stat: Rational): (Rational, Rational)

    override def statisticValuesWithProbabilities(firstSampleSize: Int,
                                                  secondSampleSize: Int): Iterable[(Rational, Double)] = {
      KSUtils.collectStatisticsWithProbabilities(firstSampleSize, secondSampleSize,
                                                 diff2stat, oneMinusP)
    }
    override def apply[T : Ordering](first: Iterable[T], second: Iterable[T]): TestResult[Rational] = {
      val (minDiff, maxDiff) = KSUtils.computeStatistics(first, second)
      val statistic = pair2stat(minDiff, maxDiff)
      val p = 1 - oneMinusP(statistic, first.size, second.size)
      TestResult(statistic = statistic,
                 p = math.min(1, p),
                 test = this,
                 firstSampleSize = first.size,
                 secondSampleSize = second.size)
    }

    private def oneMinusP(statistic: Rational, firstSampleSize: Int, secondSampleSize: Int): Double = {
      val (minDiff, maxDiff) = stat2pair(statistic)
      KSUtils.pIdenticalDistributionIsInBounds(minDiff, maxDiff, firstSampleSize, secondSampleSize)
    }
  }

  val Identical: StatisticalTest[Rational] = new KSCommon {
    override protected def diff2stat(diff: Rational): Rational = diff.abs
    override protected def pair2stat(minDiff: Rational, maxDiff: Rational): Rational = maxDiff.max(-minDiff)
    override protected def stat2pair(stat: Rational): (Rational, Rational) = (-stat, stat)
    override def name: String = "Kolmogorov-Smirnov test, null: L identical to R"
  }

  val LeftNeverDominatesRight: StatisticalTest[Rational] = new KSCommon {
    override protected def diff2stat(diff: Rational): Rational = diff.max(0)
    override protected def pair2stat(minDiff: Rational, maxDiff: Rational): Rational = maxDiff
    override protected def stat2pair(stat: Rational): (Rational, Rational) = (-2, stat) // -2 ~ "anything less than -1"
    override def name: String = "Kolmogorov-Smirnov test, null: L never dominates R"
  }

  val RightNeverDominatesLeft: StatisticalTest[Rational] = new KSCommon {
    override protected def diff2stat(diff: Rational): Rational = -diff.max(0)
    override protected def pair2stat(minDiff: Rational, maxDiff: Rational): Rational = -minDiff
    override protected def stat2pair(stat: Rational): (Rational, Rational) = (-stat, 2) // 2 ~ "anything greater than 1"
    override def name: String = "Kolmogorov-Smirnov test, null: R never dominates L"
  }
}
