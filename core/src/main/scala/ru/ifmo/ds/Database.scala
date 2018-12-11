package ru.ifmo.ds

import ru.ifmo.ds.util.DatabaseOps

abstract class Database extends DatabaseOps {
  def possibleKeys: Set[String]
  def valuesUnderKey(key: String): Set[Option[String]]
  def entries: Seq[Database.Entry]
  def foreach[T](fun: Database.Entry => T): Unit
  def hasEntries: Boolean = entries.nonEmpty

  def withMoreKeys(map: Map[String, String]): Database = {
    require(map.keySet.intersect(possibleKeys).isEmpty, "New keys should be different from existing ones")
    new Database.SimpleKeyAddingWrapper(this, map)
  }

  def filter(predicate: Database.Predicate): Database = Database(entries.filter(predicate) :_*)

  def flatten: Seq[Map[String, String]] = {
    val keys = possibleKeys
    entries.map(e => keys.flatMap(k => e.get(k).map(v => k -> v)).toMap)
  }

  def withRenamedKey(oldKey: String, newKey: String): Database = {
    def convert(oldEntry: Database.Entry): Database.Entry = {
      val mapBuilder = Map.newBuilder[String, String]
      oldEntry.foreach(kv => mapBuilder += (if (kv._1 == oldKey) newKey else kv._1) -> kv._2)
      Database.entry(mapBuilder.result())
    }
    val entryBuilder = IndexedSeq.newBuilder[Database.Entry]
    foreach(e => entryBuilder += convert(e))
    Database(entryBuilder.result() :_*)
  }
}

object Database {
  type Predicate = Entry => Boolean

  abstract class Entry {
    def contains(key: String): Boolean
    def apply(key: String): String
    def get(key: String): Option[String]
    def foreach[T](fun: ((String, String)) => T): Unit
  }

  def entry(map: Map[String, String]): Entry = new Entry {
    override def contains(key: String): Boolean = map.contains(key)
    override def apply(key: String): String = map.apply(key)
    override def get(key: String): Option[String] = map.get(key)
    override def foreach[T](fun: ((String, String)) => T): Unit = map.foreach(fun)
  }

  def apply(myEntries: Database.Entry*): Database = new Database {
    override val possibleKeys: Set[String] = {
      val builder = Set.newBuilder[String]
      myEntries.foreach(_.foreach(p => builder += p._1))
      builder.result()
    }

    override def valuesUnderKey(key: String): Set[Option[String]] = {
      val builder = Set.newBuilder[Option[String]]
      myEntries.foreach(e => builder += e.get(key))
      builder.result()
    }

    override def entries: Seq[Entry] = myEntries
    override def foreach[T](fun: Entry => T): Unit = myEntries.foreach(fun)
  }

  private class SimpleKeyAddingWrapper(base: Database, newMap: Map[String, String]) extends Database {
    private class WrapperEntry(orig: Entry) extends Entry {
      override def contains(key: String): Boolean = newMap.contains(key) || orig.contains(key)
      override def apply(key: String): String = newMap.getOrElse(key, orig(key))
      override def get(key: String): Option[String] = newMap.get(key).orElse(orig.get(key))
      override def foreach[T](fun: ((String, String)) => T): Unit = {
        newMap.foreach(fun)
        orig.foreach(fun)
      }
    }
    private val wrap: Entry => Entry = e => new WrapperEntry(e)

    override val hasEntries: Boolean = base.hasEntries
    override def possibleKeys: Set[String] = base.possibleKeys ++ newMap.keySet
    override def valuesUnderKey(key: String): Set[Option[String]] = {
      if (newMap.contains(key)) Set(newMap.get(key)) else base.valuesUnderKey(key)
    }
    override def entries: Seq[Entry] = base.entries.map(wrap)
    override def foreach[T](fun: Entry => T): Unit = base.foreach(wrap.andThen(fun))
  }

  def merge(databases: Database*): Database = {
    val keysBuilder = Set.newBuilder[String]
    val entriesBuilder = IndexedSeq.newBuilder[Database.Entry]
    for (db <- databases) {
      keysBuilder ++= db.possibleKeys
      entriesBuilder ++= db.entries
    }
    new Database {
      override val possibleKeys: Set[String] = keysBuilder.result()
      override val entries: Seq[Entry] = entriesBuilder.result()
      override def foreach[T](fun: Entry => T): Unit = entries.foreach(fun)
      override def valuesUnderKey(key: String): Set[Option[String]] = {
        val valuesBuilder = Set.newBuilder[Option[String]]
        for (db <- databases) {
          valuesBuilder ++= db.valuesUnderKey(key)
        }
        valuesBuilder.result()
      }
    }
  }
}
