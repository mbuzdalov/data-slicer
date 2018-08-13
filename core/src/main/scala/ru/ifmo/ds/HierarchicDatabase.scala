package ru.ifmo.ds

import scala.collection.mutable.ArrayBuffer
import scala.collection.{Map => ImmMap}

class HierarchicDatabase(root: HierarchicDatabase.Entry) extends Database {
  override val possibleKeys: Set[String] = {
    val builder = Set.newBuilder[String]
    root.collectKeys(builder += _)
    builder.result()
  }

  override def valuesUnderKey(key: String): Set[Option[String]] = {
    val builder = Set.newBuilder[Option[String]]
    root.collectValuesFor(key, builder += _)
    builder.result()
  }

  override val entries: Seq[Database.Entry] = {
    val builder = IndexedSeq.newBuilder[Database.Entry]
    root.foreachHierarchy(builder += _)
    builder.result()
  }

  override def foreach[T](fun: Database.Entry => T): Unit = root.foreachHierarchy(fun)
}

object HierarchicDatabase {
  class Entry(parent: Option[Entry], myKeys: ImmMap[String, String], canBeLeaf: Boolean) extends Database.Entry {
    private val myChildren = new ArrayBuffer[Entry]()

    parent foreach { p =>
      myKeys.keys.foreach(k => if (p.contains(k)) throw new IllegalArgumentException("The parent already contains key " + k))
      p.addChild(this)
    }

    private def addChild(child: Entry): Unit = myChildren += child

    override def foreach[T](fun: ((String, String)) => T): Unit = {
      myKeys.foreach(fun)
      parent.foreach(_.foreach(fun))
    }

    private[HierarchicDatabase] def childAt(index: Int): Entry = myChildren(index)
    private[HierarchicDatabase] def nChildren: Int = myChildren.size

    private[HierarchicDatabase] def foreachHierarchy[U](consumer: Entry => U): Unit = {
      if (myChildren.nonEmpty) {
        myChildren.foreach(_.foreachHierarchy(consumer))
      } else if (canBeLeaf) {
        consumer(this)
      }
    }

    private[HierarchicDatabase] def collectKeys(consumer: String => Unit): Unit = {
      myKeys.foreach(p => consumer(p._1))
      myChildren.foreach(_.collectKeys(consumer))
    }

    private[HierarchicDatabase] def collectValuesFor(key: String, consumer: Option[String] => Unit): Unit = {
      if (myKeys.contains(key)) {
        consumer(myKeys.get(key))
      } else if (canBeLeaf) {
        consumer(None)
      } else {
        myChildren.foreach(_.collectValuesFor(key, consumer))
      }
    }

    override def get(key: String): Option[String] = myKeys.get(key).orElse(parent.flatMap(_.get(key)))
    override def apply(key: String): String = get(key).getOrElse(throw new IllegalArgumentException(s"No such key $key"))
    override def contains(key: String): Boolean = myKeys.contains(key) || parent.exists(_.contains(key))
  }

  def compact(db: Database): HierarchicDatabase = {
    import scala.collection.mutable

    def wrapSingle(keys: Set[String], entry: Database.Entry, parent: Option[Entry]): Entry = {
      new Entry(parent, keys.flatMap(k => entry.get(k).map(v => k -> v)).toMap, true)
    }

    def create(keys: Set[String], entries: Seq[Database.Entry], parent: Option[Entry]): Entry = {
      require(entries.nonEmpty)
      if (entries.size == 1) wrapSingle(keys, entries.head, parent) else {
        val possibleValues = keys.map(k => (k, new mutable.HashSet[Option[String]]())).toMap
        entries.foreach(e => possibleValues.foreach(p => p._2 += e.get(p._1)))
        val (singleValued, multipleValued) = possibleValues.partition(_._2.size == 1)
        if (multipleValued.isEmpty) wrapSingle(keys, entries.head, parent) else {
          val minMultiValued = multipleValued.minBy(_._2.size)
          assert(minMultiValued._2.size > 1)
          val sliceKey = minMultiValued._1
          val newParent = if (singleValued.isEmpty && parent.isDefined) {
            // short-cutting an empty node
            parent
          } else {
            val singleValuedPairs = singleValued.flatMap(p => p._2.head.map(v => p._1 -> v))
            Some(new Entry(parent, singleValuedPairs, false))
          }
          val restOfKeys = keys.diff(singleValued.keySet)
          for ((_, slice) <- entries.groupBy(_.get(sliceKey))) {
            create(restOfKeys, slice, newParent)
          }
          newParent.get // totally safe: both branches for newParent actually return Some(x)
        }
      }
    }

    val entries = db.entries
    val root = if (entries.isEmpty) {
      new Entry(None, Map.empty, false)
    } else {
      create(db.possibleKeys, entries, None)
    }
    new HierarchicDatabase(root)
  }
}
