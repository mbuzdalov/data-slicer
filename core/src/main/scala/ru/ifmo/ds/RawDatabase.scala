package ru.ifmo.ds

abstract class RawDatabase {
  def possibleKeys: Set[String]
  def valuesUnderKey(key: String): Set[String]
  def entries: Seq[RawDatabase.Entry]
}

object RawDatabase {
  abstract class Entry {
    def contains(key: String): Boolean
    def apply(key: String): String
    def get(key: String): Option[String]
  }

  def join(databases: RawDatabase*): RawDatabase = {
    val keysBuilder = Set.newBuilder[String]
    val entriesBuilder = IndexedSeq.newBuilder[RawDatabase.Entry]
    for (db <- databases) {
      keysBuilder ++= db.possibleKeys
      entriesBuilder ++= db.entries
    }
    new RawDatabase {
      override val possibleKeys: Set[String] = keysBuilder.result()
      override val entries: Seq[Entry] = entriesBuilder.result()
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
