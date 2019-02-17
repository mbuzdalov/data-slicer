package ru.ifmo.ds.stat

import spire.math.Rational

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
    val firstR = (0 to firstSampleSize).map(x => Rational(x, firstSampleSize))
    val secondR = (0 to secondSampleSize).map(x => Rational(x, secondSampleSize))
    val stats = (for (f <- firstR; s <- secondR) yield (f - s).abs).distinct.sorted
    val pvals = stats.map(s => 1 - pSmirnovDoesNotExceedTwoSided(s, firstSampleSize, secondSampleSize))
    val size = stats.size

    def diff(i: Int): Double = if (i + 1 == size) pvals(i) else pvals(i) - pvals(i + 1)

    stats.indices.map(i => stats(i) -> diff(i))
  }

  // adapted from sources of R 3.4.1: src/library/stats/src/ks.c
  private[stat] object R {
    final def pSmirnov2x(stat: Rational, m: Int, n: Int): Double = {
      if (m > n) pSmirnov2x(stat, n, m) else {
        val r = Array.tabulate(n + 1)(j => Rational(j, n))
        val u = Array.tabulate(n + 1)(j => if (r(j) >= stat) 0.0 else 1.0)

        for (i <- 1 to m) {
          val im = Rational(i, m)
          val w = i / (i.toDouble + n)
          u(0) = if (im >= stat) 0 else w * u(0)
          for (j <- 1 to n) {
            u(j) = if ((im - r(j)).abs >= stat) 0 else w * u(j) + u(j - 1)
          }
        }

        u(n)
      }
    }
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
    * Once we did that, `u(m, n)` will report the desired value.
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
  final def pSmirnovDoesNotExceedTwoSided(stat: Rational, m: Int, n: Int): Double = {
    if (m > n) pSmirnovDoesNotExceedTwoSided(stat, n, m) else {
      val u = Array.ofDim[Double](n + 1)

      u(0) = 1.0
      for (i <- 0 to m) {
        val im = Rational(i, m)
        val jMinRaw = (im - stat) * n
        val jMaxRaw = (im + stat) * n
        val jMin = math.max(0, if (jMinRaw.isWhole()) jMinRaw.toInt + 1 else jMinRaw.ceil.toInt)
        val jMax = math.min(n, if (jMaxRaw.isWhole()) jMaxRaw.toInt - 1 else jMaxRaw.floor.toInt)

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
    val p = 1 - pSmirnovDoesNotExceedTwoSided(statistic, asSize, bsSize)
    TestResult(statistic = statistic,
               p = math.min(1, p),
               test = this,
               firstSampleSize = asSize,
               secondSampleSize = bsSize)
  }
}
