package ru.ifmo.ds.util

object Extensions {
  implicit class SeqOps[T](val seq: Seq[T]) extends AnyVal {
    def groupByOption[U](fun: T => Option[U]): Map[U, Seq[T]] = {
      seq.groupBy(fun).filterKeys(_.nonEmpty).map(p => (p._1.get, p._2))
    }
  }
}
