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
}
