package ru.ifmo.ds.ops

import ru.ifmo.ds.Database
import ru.ifmo.ds.stat.ApproximateKolmogorovSmirnov

object FindDifferences {
  trait DifferenceListener {
    def keyValuesDoNotMatch(slice: Map[String, Option[String]],
                            key: String, onlyLeft: Set[Option[String]], onlyRight: Set[Option[String]]): Unit
    def kolmogorovSmirnovFailure(slice: Map[String, Option[String]], key: String,
                                 leftValues: Seq[String], rightValues: Seq[String], exception: Throwable): Unit
    def kolmogorovSmirnovResult(slice: Map[String, Option[String]], key: String,
                                leftValues: Seq[Double], rightValues: Seq[Double], pValue: Double): Unit
  }

  private[this] def traverseImpl(left: Database, right: Database, slice: Map[String, Option[String]],
                                 categoryKeys: Seq[String], valueKey: String, listener: DifferenceListener): Unit = {
    if (categoryKeys.isEmpty) {
      val rawLeft = left.entries.map(_(valueKey))
      val rawRight = right.entries.map(_(valueKey))
      try {
        val leftValues = rawLeft.map(_.toDouble)
        val rightValues = rawRight.map(_.toDouble)
        val ks = ApproximateKolmogorovSmirnov(leftValues, rightValues)
        listener.kolmogorovSmirnovResult(slice, valueKey, leftValues, rightValues, ks.p)
      } catch {
        case th: Throwable => listener.kolmogorovSmirnovFailure(slice, valueKey, rawLeft, rawRight, th)
      }
    } else {
      val key = categoryKeys.head
      val leftOptions = left.entries.map(_.get(key)).toSet
      val rightOptions = right.entries.map(_.get(key)).toSet
      if (leftOptions != rightOptions) {
        val onlyLeft = leftOptions.diff(rightOptions)
        val onlyRight = rightOptions.diff(leftOptions)
        listener.keyValuesDoNotMatch(slice, key, onlyLeft, onlyRight)
      }
      val commonOptions = leftOptions.intersect(rightOptions)
      if (commonOptions.nonEmpty) {
        val newKeys = categoryKeys.tail
        for (o <- commonOptions) {
          val newLeft = left.filter(_.get(key) == o)
          val newRight = right.filter(_.get(key) == o)
          traverseImpl(newLeft, newRight, slice.updated(key, o), newKeys, valueKey, listener)
        }
      }
    }
  }

  def traverse(left: Database, right: Database, categoryKeys: Seq[String],
               valueKey: String, listener: DifferenceListener): Unit = {
    traverseImpl(left, right, Map.empty, categoryKeys, valueKey, listener)
  }

  def traverse(db: Database, differenceKey: String, leftValue: Option[String], rightValue: Option[String],
               categoryKeys: Seq[String], valueKey: String, listener: DifferenceListener): Unit = {
    val ldb = db.filter(e => e.get(differenceKey) == leftValue && e.contains(valueKey))
    val rdb = db.filter(e => e.get(differenceKey) == rightValue && e.contains(valueKey))
    traverseImpl(ldb, rdb, Map.empty, categoryKeys, valueKey, listener)
  }
}
