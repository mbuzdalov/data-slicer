package ru.ifmo.ds.util

object Extensions {
  private def unOption[A, B](p: (Option[A], B)): (A, B) = (p._1.get, p._2)

  implicit class SeqOps[T](val seq: Seq[T]) extends AnyVal {
    def groupByOption[U](fun: T => Option[U]): Map[U, Seq[T]] = {
      seq.groupBy(fun).view.filterKeys(_.nonEmpty).map(unOption).toMap
    }
  }
}
