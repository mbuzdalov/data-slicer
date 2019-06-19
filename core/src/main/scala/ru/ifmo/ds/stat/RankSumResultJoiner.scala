package ru.ifmo.ds.stat

import scala.Ordering.Double.IeeeOrdering

object RankSumResultJoiner {
  private def joinImpl[T](results: Iterator[TestResult[T]], prevArray: Array[Double], rankSum: Int)
                         (implicit ordering: Ordering[T]): Double = {
    if (results.hasNext) {
      val result = results.next()
      val possibleStats = result
        .test
        .statisticValuesWithProbabilities(result.firstSampleSize, result.secondSampleSize)
        .toIndexedSeq.sorted
      val pSize = prevArray.length
      val nStats = possibleStats.size
      val currArray = new Array[Double](pSize + nStats - 1)
      var i = 0
      var rank = -1
      while (i < nStats) {
        val stat = possibleStats(i)
        if (stat._1 == result.statistic) {
          assert(rank == -1, "There are two equal statistic values")
          rank = i
        }
        val prob = stat._2
        var j = 0
        while (j < pSize) {
          currArray(i + j) += prob * prevArray(j)
          j += 1
        }

        i += 1
      }
      assert(rank != -1, "The current statistic was not found")
      joinImpl(results, currArray, rankSum + rank)
    } else prevArray.drop(rankSum).sum
  }

  def join[T](results: Iterable[TestResult[T]])(implicit ordering: Ordering[T]): Double = {
    joinImpl(results.iterator, Array(1.0), 0)
  }
}
