package ru.ifmo.ds.util

import ru.ifmo.ds.Database

trait DatabaseOps { self: Database =>
  def groupMap2D[X, Y, Z](extractX: Database.Entry => Option[X],
                          extractY: Database.Entry => Option[Y],
                          extractZ: Database.Entry => Option[Z]): Map[X, Map[Y, Seq[Z]]] = {
    import Extensions._
    entries.groupByOption(extractX).mapValues(_.groupByOption(extractY).mapValues(_.flatMap(e => extractZ(e))))
  }

  def partitionByKeys[LeafData](keys: Seq[String],
                                      leafFunction: Database => LeafData): PartitionTree[String, LeafData] = {
    if (keys.isEmpty || !self.hasEntries) {
      // Empty database, call the function immediately
      PartitionTree.Leaf(leafFunction(self))
    } else {
      val currentKey = keys.head
      val otherKeys = keys.tail
      val options = self.valuesUnderKey(currentKey)
      assert(options.nonEmpty)
      if (options.size == 1) {
        partitionByKeys(otherKeys, leafFunction)
      } else {
        val grouped = self.entries.groupBy(_.get(currentKey))
        val withProperName = grouped.map(p => (p._1.getOrElse("<none>"), Database(p._2 :_*)))
        val sorted = withProperName.toIndexedSeq.sortBy(_._1)(OrderingForStringWithNumbers.SpecialDotTreatment)
        PartitionTree.Node(sorted.map(p => (p._1, p._2.partitionByKeys(otherKeys, leafFunction))))
      }
    }
  }
}
