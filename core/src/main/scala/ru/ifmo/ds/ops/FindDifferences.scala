package ru.ifmo.ds.ops

import scala.Ordering.Double.IeeeOrdering

import spire.math.Rational

import ru.ifmo.ds.Database
import ru.ifmo.ds.stat.{KolmogorovSmirnov, TestResult}
import ru.ifmo.ds.util.OrderingForStringWithNumbers

object FindDifferences {
  trait DifferenceListener {
    def keyValuesDoNotMatch(slice: Map[String, Option[String]],
                            key: String, onlyLeft: Set[Option[String]], onlyRight: Set[Option[String]]): Unit
    def kolmogorovSmirnovFailure(slice: Map[String, Option[String]], key: String,
                                 leftValues: Seq[String], rightValues: Seq[String], exception: Throwable): Unit
    def kolmogorovSmirnovResult(slice: Map[String, Option[String]], key: String,
                                leftValues: Seq[Double], rightValues: Seq[Double],
                                result: TestResult[Rational]): Unit
    def sliceStatistics(slice: Map[String, Option[String]], key: String,
                        statistics: Seq[TestResult[Rational]]): Unit
  }

  sealed trait Result
  case class NonMatchedKeys(slice: Map[String, Option[String]], key: String,
                            onlyLeft: Set[Option[String]], onlyRight: Set[Option[String]]) extends Result
  case class KolmogorovSmirnovFailure(slice: Map[String, Option[String]], key: String,
                                      leftValues: Seq[String], rightValues: Seq[String],
                                      exception: Throwable) extends Result
  case class KolmogorovSmirnovResult(slice: Map[String, Option[String]], key: String,
                                     leftValues: Seq[Double], rightValues: Seq[Double],
                                     result: TestResult[Rational]) extends Result

  private[this] implicit val optionStringOrdering: Ordering[Option[String]] =
    Ordering.Option(OrderingForStringWithNumbers.SpecialDotTreatment)

  private[this] def traverseImpl(left: Database, right: Database, slice: Map[String, Option[String]],
                                 categoryKeys: Seq[String], valueKey: String, listener: DifferenceListener
                                ): IndexedSeq[TestResult[Rational]] = {
    if (categoryKeys.isEmpty) {
      val rawLeft = left.entries.map(_(valueKey))
      val rawRight = right.entries.map(_(valueKey))
      try {
        val leftValues = rawLeft.map(_.toDouble)
        val rightValues = rawRight.map(_.toDouble)
        val ks = KolmogorovSmirnov.Identical(leftValues, rightValues)
        listener.kolmogorovSmirnovResult(slice, valueKey, leftValues, rightValues, ks)
        IndexedSeq(ks)
      } catch {
        case th: Throwable =>
          listener.kolmogorovSmirnovFailure(slice, valueKey, rawLeft, rawRight, th)
          IndexedSeq.empty
      }
    } else {
      val key = categoryKeys.head
      val leftOptions = left.valuesUnderKey(key)
      val rightOptions = right.valuesUnderKey(key)
      if (leftOptions != rightOptions) {
        val onlyLeft = leftOptions.diff(rightOptions)
        val onlyRight = rightOptions.diff(leftOptions)
        listener.keyValuesDoNotMatch(slice, key, onlyLeft, onlyRight)
      }
      val commonOptions = leftOptions.intersect(rightOptions).toIndexedSeq.sorted
      val result = if (commonOptions.nonEmpty) {
        val newKeys = categoryKeys.tail
        commonOptions flatMap { o =>
          val newLeft = left.filter(_.get(key) == o)
          val newRight = right.filter(_.get(key) == o)
          traverseImpl(newLeft, newRight, slice.updated(key, o), newKeys, valueKey, listener)
        }
      } else IndexedSeq.empty
      listener.sliceStatistics(slice, valueKey, result)
      result
    }
  }

  private[this] class Builder extends DifferenceListener {
    private[this] val builder = IndexedSeq.newBuilder[Result]

    override def sliceStatistics(slice: Map[String, Option[String]], key: String,
                                 statistics: Seq[TestResult[Rational]]): Unit = {}

    override def keyValuesDoNotMatch(slice: Map[String, Option[String]], key: String,
                                     onlyLeft: Set[Option[String]], onlyRight: Set[Option[String]]): Unit =
      builder += NonMatchedKeys(slice, key, onlyLeft, onlyRight)

    override def kolmogorovSmirnovFailure(slice: Map[String, Option[String]], key: String, leftValues: Seq[String],
                                          rightValues: Seq[String], exception: Throwable): Unit = {
      builder += KolmogorovSmirnovFailure(slice, key, leftValues, rightValues, exception)
    }

    override def kolmogorovSmirnovResult(slice: Map[String, Option[String]], key: String, leftValues: Seq[Double],
                                         rightValues: Seq[Double], result: TestResult[Rational]): Unit = {
      builder += KolmogorovSmirnovResult(slice, key, leftValues, rightValues, result)
    }

    def result(): IndexedSeq[Result] = builder.result()
  }

  def traverse(left: Database, right: Database, categoryKeys: Seq[String],
               valueKey: String, listener: DifferenceListener): Unit = {
    traverseImpl(left.filter(_.contains(valueKey)), right.filter(_.contains(valueKey)),
                 Map.empty, categoryKeys, valueKey, listener)
  }

  def traverse(db: Database, differenceKey: String,
               leftPredicate: Option[String] => Boolean, rightPredicate: Option[String] => Boolean,
               categoryKeys: Seq[String], valueKey: String, listener: DifferenceListener): Unit = {
    val ldb = db.filter(e => leftPredicate(e.get(differenceKey)) && e.contains(valueKey))
    val rdb = db.filter(e => rightPredicate(e.get(differenceKey)) && e.contains(valueKey))
    traverseImpl(ldb, rdb, Map.empty, categoryKeys, valueKey, listener)
  }

  def collect(left: Database, right: Database, categoryKeys: Seq[String],
              valueKey: String, listener: DifferenceListener): Seq[Result] = {
    val b = new Builder
    traverse(left, right, categoryKeys, valueKey, b)
    b.result()
  }

  def collect(db: Database, differenceKey: String,
              leftPredicate: Option[String] => Boolean, rightPredicate: Option[String] => Boolean,
              categoryKeys: Seq[String], valueKey: String, listener: DifferenceListener): Seq[Result] = {
    val b = new Builder
    traverse(db, differenceKey, leftPredicate, rightPredicate, categoryKeys, valueKey, b)
    b.result()
  }
}
