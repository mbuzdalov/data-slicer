package ru.ifmo.ds.stat

/**
  * The Kolmogorov-Smirnov test to be used while detecting differences
  */
object KolmogorovSmirnov {
  case class Result(p: Double, d: Double)

  // adapted from sources of R 3.4.1: src/library/stats/src/ks.c
  private[this] def pSmirnov2x(stat: Double, m: Int, n: Int): Double = {
    if (m > n) pSmirnov2x(stat, n, m) else {
      val md = m.toDouble
      val nd = n.toDouble
      val q = (0.5 + math.floor(stat * md * nd - 1e-7)) / (md * nd)
      val u = Array.tabulate(n + 1)(j => if (j / nd > q) 0.0 else 1.0)

      for (i <- 1 to m) {
        val w = i / (i.toDouble + n)
        u(0) = if (i / md > q) 0 else w * u(0)
        for (j <- 1 to n) {
          u(j) = if (math.abs(i / md - j / nd) > q) 0 else w * u(j) + u(j - 1)
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

    def go(ai: Int, bi: Int, diff: Double): Double = {
      val maxDiff = math.max(diff, math.abs(ai.toDouble / asSize - bi.toDouble / bsSize))

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
      approx(statistic * math.sqrt(asSize * bsSize / (0.0 + asSize + bsSize)))
    }
    Result(p = math.min(1, p), d = statistic)
  }
}
