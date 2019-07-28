package ru.ifmo.ds.stat

import java.util.concurrent.ThreadLocalRandom

object EvolutionaryGroupTestingValidation {
  private val rng = ThreadLocalRandom.current()

  private def initRandomly(a: Array[Boolean]): Int = {
    var i = 0
    var f = 0
    val n = a.length
    while (i < n) {
      a(i) = rng.nextBoolean()
      if (a(i)) {
        f += 1
      }
      i += 1
    }
    f
  }

  private def rls(n: Int): Int = {
    val parent = new Array[Boolean](n)
    var parentFitness = initRandomly(parent)
    var iterations = 1
    while (parentFitness < n) {
      val index = rng.nextInt(n)
      if (!parent(index)) {
        parent(index) = true
        parentFitness += 1
      }
      iterations += 1
    }
    iterations
  }

  private def rlsInit(n: Int): Int = {
    val parent, child = new Array[Boolean](n)
    var parentFitness = initRandomly(parent)
    var iterations = 1
    val limit = math.sqrt(n / math.log(n)).ceil.toInt
    while (parentFitness < n && iterations < limit) {
      val altFitness = initRandomly(child)
      if (parentFitness <= altFitness) {
        parentFitness = altFitness
        System.arraycopy(child, 0, parent, 0, n)
      }
      iterations += 1
    }
    while (parentFitness < n) {
      val index = rng.nextInt(n)
      if (!parent(index)) {
        parent(index) = true
        parentFitness += 1
      }
      iterations += 1
    }
    iterations
  }

  private def compareDiff(n: Int, times: Int): Unit = {
    print("  diff: [")
    val base = Array.tabulate(n)(s => Array.fill(times)(rls(s + 1)))
    print(".")
    val diff = Array.tabulate(n)(s => Array.fill(times)(rlsInit(s + 1)))
    print(".")
    val comparisons = base.lazyZip(diff).map((a, b) => KolmogorovSmirnov.TwoSided(a, b))
    println("] " + RankSumResultJoiner.join(comparisons))
  }

  private def compareSame(n: Int, times: Int): Unit = {
    print("  same: [")
    val base = Array.tabulate(n)(s => Array.fill(times)(rls(s + 1)))
    print(".")
    val diff = Array.tabulate(n)(s => Array.fill(times)(rls(s + 1)))
    print(".")
    val comparisons = base.lazyZip(diff).map((a, b) => KolmogorovSmirnov.TwoSided(a, b))
    println("] " + RankSumResultJoiner.join(comparisons))
  }

  def main(args: Array[String]): Unit = {
    for (n <- 100 to 500 by 100; times <- 100 to 500 by 100) {
      println(s"n = $n, times = $times:")
      for (_ <- 0 until 5) {
        compareDiff(n, times)
        compareSame(n, times)
        println("  ------")
      }
    }
  }
}
