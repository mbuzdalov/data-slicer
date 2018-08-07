package ru.ifmo.ds.stat

import ru.ifmo.ds.Database

/**
  * An approximate Kolmogorov-Smirnov test to be used while detecting differences
  */
object ApproximateKolmogorovSmirnov {
  case class Result(p: Double, d: Double)

  def apply[T : Ordering](a: Iterable[T], b: Iterable[T]): Result = {
    val as = a.toIndexedSeq.sorted
    val bs = b.toIndexedSeq.sorted

    val ord = implicitly[Ordering[T]]

    def nextGreaterOrSize(a: IndexedSeq[T], v: T, from: Int): Int = {
      val idx = a.indexWhere(i => ord.compare(v, i) < 0, from)
      if (idx == -1) a.size else idx
    }

    def go(ai: Int, bi: Int, diff: Double): Double = {
      val maxDiff = math.max(diff, math.abs(ai.toDouble / as.length - bi.toDouble / bs.length))

      if (as.size == ai || bs.size == bi) maxDiff else {
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
    val p = 2 * math.exp(-2 * statistic * statistic * as.length * bs.length / (0.0 + as.length + bs.length))
    Result(p = p, d = statistic)
  }

  def runPairwise(db: Database, valueKey: String, resultKey: String, groupKeys: Set[String],
                  oppositionKey: String, leftValue: String, rightValue: String): Database = {
    import scala.collection.mutable

    class BufferPair {
      val left, right = new mutable.ArrayBuffer[Double]()
    }

    val map = new mutable.HashMap[Seq[Option[String]], BufferPair]()
    val keys = groupKeys.toIndexedSeq

    def getFor(e: Database.Entry): BufferPair = map.getOrElseUpdate(keys.map(e.get), new BufferPair)

    for (e <- db) {
      if (e.contains(valueKey)) {
        e.get(oppositionKey) match {
          case Some(`leftValue`)  => getFor(e).left  += e(valueKey).toDouble
          case Some(`rightValue`) => getFor(e).right += e(valueKey).toDouble
          case _ =>
        }
      }
    }

    Database(map.toIndexedSeq.map {
      case (values, bp) =>
        val entryMap = new mutable.HashMap[String, String]()
        for ((k, i) <- keys.zipWithIndex) {
          values(i).foreach(v => entryMap += k -> v)
        }
        entryMap += resultKey -> ApproximateKolmogorovSmirnov(bp.left, bp.right).p.toString
        Database.entry(entryMap.toMap)
    } :_*)
  }
}
