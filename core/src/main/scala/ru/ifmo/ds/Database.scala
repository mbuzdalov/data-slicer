package ru.ifmo.ds

abstract class Database {
  def possibleKeys: Set[String]
  def valuesUnderKey(key: String): Set[String]
  def entries: Seq[Database.Entry]
  def foreach[T](fun: Database.Entry => T): Unit

  def withMoreKeys(map: Map[String, String]): Database = {
    require(map.keySet.diff(possibleKeys).isEmpty, "New keys should be different from existing ones")
    new Database.SimpleKeyAddingWrapper(this, map)
  }

  def filter(predicate: Database.Predicate): Database = Database(entries.filter(predicate) :_*)

  def flatten: Seq[Map[String, String]] = {
    val keys = possibleKeys
    entries.map(e => keys.flatMap(k => e.get(k).map(v => k -> v)).toMap)
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

  def apply(myEntries: Database.Entry*): Database = new Database {
    override val possibleKeys: Set[String] = {
      val builder = Set.newBuilder[String]
      myEntries.foreach(_.foreach(p => builder += p._1))
      builder.result()
    }

    override def valuesUnderKey(key: String): Set[String] = {
      val builder = Set.newBuilder[String]
      myEntries.foreach(e => builder ++= e.get(key))
      builder.result()
    }

    override def entries: Seq[Entry] = myEntries
    override def foreach[T](fun: Entry => T): Unit = myEntries.foreach(fun)
  }

  private class SimpleKeyAddingWrapper(base: Database, map: Map[String, String]) extends Database {
    private class WrapperEntry(orig: Entry) extends Entry {
      override def contains(key: String): Boolean = map.contains(key) || orig.contains(key)
      override def apply(key: String): String = map.getOrElse(key, orig(key))
      override def get(key: String): Option[String] = map.get(key).orElse(orig.get(key))
      override def foreach[T](fun: ((String, String)) => T): Unit = {
        map.foreach(fun)
        orig.foreach(fun)
      }
    }
    private val wrap: Entry => Entry = e => new WrapperEntry(e)

    override def possibleKeys: Set[String] = base.possibleKeys ++ map.keySet
    override def valuesUnderKey(key: String): Set[String] = if (map.contains(key)) Set(map(key)) else base.valuesUnderKey(key)
    override def entries: Seq[Entry] = base.entries.map(wrap)
    override def foreach[T](fun: Entry => T): Unit = base.foreach(wrap.andThen(fun))
  }

  def join(databases: Database*): Database = {
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
      override def valuesUnderKey(key: String): Set[String] = {
        val valuesBuilder = Set.newBuilder[String]
        for (db <- databases) {
          valuesBuilder ++= db.valuesUnderKey(key)
        }
        valuesBuilder.result()
      }
    }
  }
}
