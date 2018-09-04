package ru.ifmo.ds.util

import ru.ifmo.ds.Database

trait DatabaseOps { self: Database =>
  def groupMap2D[X, Y, Z](extractX: Database.Entry => Option[X],
                          extractY: Database.Entry => Option[Y],
                          extractZ: Database.Entry => Option[Z]): Map[X, Map[Y, Seq[Z]]] = {
    import Extensions._
    entries.groupByOption(extractX).mapValues(_.groupByOption(extractY).mapValues(_.flatMap(e => extractZ(e))))
  }
}
