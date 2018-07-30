package ru.ifmo.ds

abstract class Database {
  def possibleKeys: Set[String]
  def valuesUnderKey(key: String): Set[String]
  def entries: Seq[Database.Entry]
  def foreach[T](fun: Database.Entry => T): Unit
}

object Database {
  abstract class Entry {
    def contains(key: String): Boolean
    def apply(key: String): String
    def get(key: String): Option[String]
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
