package ru.ifmo.ds.stat

import spire.math.Rational

/**
  * The Kolmogorov-Smirnov test to be used while detecting differences
  */
object KolmogorovSmirnov {
  case class Result(p: Double, d: Rational, firstSampleSize: Int, secondSampleSize: Int)

  // adapted from sources of R 3.4.1: src/library/stats/src/ks.c
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

  final def pSmirnov2y(stat: Rational, m: Int, n: Int): Double = {
    if (m > n) pSmirnov2y(stat, n, m) else {
      val u = Array.ofDim[Double](n + 1)

      u(0) = 1.0
      for (i <- 0 to m) {
        val im = Rational(i, m)
        val jMinRaw = (im - stat) * n
        val jMaxRaw = (im + stat) * n
        val jMin = math.max(0, if (jMinRaw.isWhole()) jMinRaw.toInt + 1 else jMinRaw.ceil.toInt)
        val jMax = math.min(n, if (jMaxRaw.isWhole()) jMaxRaw.toInt - 1 else jMaxRaw.floor.toInt)
        for (j <- jMin to jMax) {
          val rowMul = (n - j).toDouble / (n - j + m - i)
          if (jMax > j) {
            u(j + 1) += u(j) * rowMul
          }
          if (m > i) {
            u(j) *= 1 - rowMul
          }
        }
      }
      u(n)
    }
  }

  private[this] def approx(stat: Double): Double = {
    val atZero = math.exp(-2 * stat * stat)
    def impl(soFar: Double, k: Double): Double = {
      val addend = 2 * math.pow(atZero, k * k) * (2 * (k % 2) - 1)
      if (addend == 0) soFar else impl(soFar + addend, k + 1)
    }
    impl(0, 1)
  }

  def apply[T : Ordering](a: Iterable[T], b: Iterable[T], strict: Boolean = true): Result = {
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
    val p = if (strict) {
      1 - pSmirnov2x(statistic, asSize, bsSize)
    } else {
      approx(statistic.toDouble * math.sqrt(asSize * bsSize / (0.0 + asSize + bsSize)))
    }
    Result(p = math.min(1, p), d = statistic, firstSampleSize = asSize, secondSampleSize = bsSize)
  }

  private[this] val sizeCache = new scala.collection.mutable.HashMap[(Int, Int), Array[Double]]

  def rankSumOnMultipleOutcomes(stats: Seq[Result]): Double = {
    if (stats.isEmpty) 1.0 else {
      val n = stats.head.firstSampleSize
      val m = stats.head.secondSampleSize
      require(stats.forall(s => s.firstSampleSize == n && s.secondSampleSize == m),
        "sample sizes in each experiment must be equal on the corresponding sides")
      val possibleStats = (for (x <- 0 to n; y <- 0 to m) yield (Rational(x, n) - Rational(y, m)).abs).distinct.sortBy(-_)

      val statIndices = possibleStats.zipWithIndex.toMap
      val inputRankSum = stats.map(s => statIndices(s.d)).sum

      val dpArray = sizeCache.getOrElseUpdate(n -> m, {
        val possiblePVals = possibleStats.map(s => 1 - pSmirnov2x(s, n, m))
        val possibleProbs = possiblePVals.indices.map(i => possiblePVals(i) - (if (i > 0) possiblePVals(i - 1) else 0))
        require(math.abs(possibleProbs.sum - 1) < 1e-9)

        val dpCurr, dpNext = Array.ofDim[Double](stats.size * (possibleProbs.size - 1) + 1)
        dpCurr(0) = 1.0
        for (_ <- stats.indices) {
          java.util.Arrays.fill(dpNext, 0.0)
          var i = dpCurr.length - 1
          while (i >= 0) {
            if (dpCurr(i) > 0) {
              var j = 0
              val jMax = possibleProbs.size
              while (j < jMax) {
                dpNext(i + j) += dpCurr(i) * possibleProbs(j)
                j += 1
              }
            }
            i -= 1
          }
          System.arraycopy(dpNext, 0, dpCurr, 0, dpCurr.length)
        }
        dpCurr
      })

      (0 to inputRankSum).view.map(dpArray).sum
    }
  }
}
