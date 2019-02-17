package ru.ifmo.ds.srv

import spire.math.Rational

import ru.ifmo.ds.ops.FindDifferences.DifferenceListener
import ru.ifmo.ds.stat.{RankSumResultJoiner, TestResult}

class CompareListener(p: Double, comparedEntityKey: String) extends DifferenceListener {
  private val singleKeySet = Set(comparedEntityKey)
  private val differingKeys, sameKeys = IndexedSeq.newBuilder[String]
  override def keyValuesDoNotMatch(slice: Map[String, Option[String]], key: String,
                                   onlyLeft: Set[Option[String]],
                                   onlyRight: Set[Option[String]]): Unit = {
    // It can be that the new commit features new keys which did not exist yet. These need to be added.
    // It can be that the new commit deletes some of the old keys. These need not to be added.
    if (key == comparedEntityKey) {
      onlyRight.foreach(differingKeys ++= _)
    }
  }
  override def kolmogorovSmirnovFailure(slice: Map[String, Option[String]],
                                        key: String, leftValues: Seq[String], rightValues: Seq[String],
                                        exception: Throwable): Unit = throw exception
  override def kolmogorovSmirnovResult(slice: Map[String, Option[String]],
                                       key: String, leftValues: Seq[Double], rightValues: Seq[Double],
                                       result: TestResult[Rational]): Unit = {}
  override def sliceStatistics(slice: Map[String, Option[String]],
                               key: String, statistics: Seq[TestResult[Rational]]): Unit = {
    if (slice.keySet == singleKeySet) {
      slice(comparedEntityKey) match {
        case None =>
        case Some(value) =>
          val stat = RankSumResultJoiner.join(statistics)
          if (stat < p) {
            differingKeys += s"$value $stat"
          } else {
            sameKeys += s"#$value $stat"
          }
      }
    }
  }

  def result(): IndexedSeq[String] = differingKeys.result() ++ sameKeys.result()
}
