package ru.ifmo.ds

import scala.collection.mutable.ArrayBuffer
import scala.collection.{Map => ImmMap}

class HierarchicDatabase(root: HierarchicDatabase.Entry) extends Database {
  override val possibleKeys: Set[String] = {
    val builder = Set.newBuilder[String]
    root.collectKeys(builder += _)
    builder.result()
  }

  override def valuesUnderKey(key: String): Set[String] = {
    val builder = Set.newBuilder[String]
    root.collectValuesFor(key, builder += _)
    builder.result()
  }

  override val entries: Seq[Database.Entry] = {
    val builder = IndexedSeq.newBuilder[Database.Entry]
    root.collectLeaves(builder += _)
    builder.result()
  }
}

object HierarchicDatabase {
  class Entry(parent: Option[Entry], myKeys: ImmMap[String, String], canBeLeaf: Boolean) extends Database.Entry {
    private val myChildren = new ArrayBuffer[Entry]()

    parent foreach { p =>
      myKeys.keys.foreach(k => if (p.contains(k)) throw new IllegalArgumentException("The parent already contains key " + k))
      p.myChildren += this
    }

    def collectLeaves(consumer: Entry => Unit): Unit = {
      if (!myChildren.isEmpty) {
        myChildren.foreach(_.collectLeaves(consumer))
      } else if (canBeLeaf) {
        consumer(this)
      }
    }

    def collectKeys(consumer: String => Unit): Unit = {
      myKeys.foreach(p => consumer(p._1))
      myChildren.foreach(_.collectKeys(consumer))
    }

    def collectValuesFor(key: String, consumer: String => Unit): Unit = {
      if (myKeys.contains(key)) {
        consumer(myKeys(key))
      } else {
        myChildren.foreach(_.collectValuesFor(key, consumer))
      }
    }

    override def get(key: String): Option[String] = myKeys.get(key).orElse(parent.flatMap(_.get(key)))
    override def apply(key: String): String = get(key).getOrElse(throw new IllegalArgumentException(s"No such key $key"))
    override def contains(key: String): Boolean = myKeys.contains(key) || parent.exists(_.contains(key))
  }
}
