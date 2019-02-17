package ru.ifmo.ds.stat

import spire.math.Rational

/**
  * The approximate version of the Kolmogorov-Smirnov test to be used while detecting differences
  */
object KolmogorovSmirnovApprox extends StatisticalTest[Rational] {
  private def approx(stat: Double): Double = {
    val atZero = math.exp(-2 * stat * stat)
    def impl(soFar: Double, k: Double): Double = {
      val addend = 2 * math.pow(atZero, k * k) * (2 * (k % 2) - 1)
      if (addend == 0) soFar else impl(soFar + addend, k + 1)
    }
    impl(0, 1)
  }

  override def apply[T : Ordering](a: Iterable[T], b: Iterable[T]): TestResult[Rational] = {
    val as = a.toIndexedSeq.sorted
    val bs = b.toIndexedSeq.sorted
    val asSize = as.size
    val bsSize = bs.size

    val ord = implicitly[Ordering[T]]

    def nextGreaterOrSize(a: IndexedSeq[T], v: T, from: Int): Int = {
      val idx = a.indexWhere(i => ord.compare(v, i) < 0, from)
      if (idx == -1) a.size else idx
    }

    def go(ai: Int, bi: Int, diff: Rational): Rational = {
      val maxDiff = (Rational(ai, asSize) - Rational(bi, bsSize)).abs.max(diff)

      if (asSize == ai || bsSize == bi) maxDiff else {
        val av = as(ai)
        val bv = bs(bi)
        val cmp = ord.compare(av, bv)
        if (cmp < 0) {
          go(nextGreaterOrSize(as, av, ai), bi, maxDiff)
        } else if (cmp > 0) {
          go(ai, nextGreaterOrSize(bs, bv, bi), maxDiff)
        } else {
          go(nextGreaterOrSize(as, av, ai), nextGreaterOrSize(bs, bv, bi), maxDiff)
        }
      }
    }

    val statistic = go(0, 0, 0)
    val p = approx(statistic.toDouble * math.sqrt(asSize * bsSize / (0.0 + asSize + bsSize)))
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
    val firstR = (0 to firstSampleSize).map(x => Rational(x, firstSampleSize))
    val secondR = (0 to secondSampleSize).map(x => Rational(x, secondSampleSize))
    val stats = (for (f <- firstR; s <- secondR) yield (f - s).abs).distinct.sorted
    val pvals = stats.map(s => approx(s.toDouble))
    val size = stats.size

    def diff(i: Int): Double = if (i + 1 == size) pvals(i) else pvals(i) - pvals(i + 1)

    stats.indices.map(i => stats(i) -> diff(i))
  }
}
