package ru.ifmo.ds

import scala.collection.{Map => BaseMap}
import scala.collection.{Map => ImmMap}
import scala.collection.mutable.{ArrayBuffer, HashMap => MuHashMap}

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
  abstract class Entry(parent: Option[Entry]) extends Database.Entry {
    private val myChildren = new ArrayBuffer[Entry]()

    parent.foreach(_.myChildren += this)

    protected def myKeys: BaseMap[String, String]

    def collectLeaves(consumer: Entry => Unit): Unit = {
      if (myChildren.isEmpty) {
        consumer(this)
      } else {
        myChildren.foreach(_.collectLeaves(consumer))
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

  class ImmutableEntry(parent: Option[Entry], protected val myKeys: ImmMap[String, String]) extends Entry(parent)

  class MutableEntry(parent: Option[Entry]) extends Entry(parent) {
    private[this] val myKeysImpl = new MuHashMap[String, String]
    override protected def myKeys: BaseMap[String, String] = myKeysImpl

    def put(key: String, value: String): Unit = {
      myKeysImpl.update(key, value)
    }
  }
}
