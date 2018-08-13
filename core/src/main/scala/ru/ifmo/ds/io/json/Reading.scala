package ru.ifmo.ds.io.json

import java.io.Reader

import scala.collection.mutable.{ArrayBuffer, HashMap => MuHashMap, HashSet => MuHashSet}

import com.google.gson.stream.{JsonReader, JsonToken}

import ru.ifmo.ds.io.Json.ParseException
import ru.ifmo.ds.{Database, HierarchicDatabase}

object Reading {
  def read(reader: Reader, moreKeys: Map[String, String]): Database = {
    val jsonReader = new JsonReader(reader)
    val rawRoot = jsonReader.peek() match {
      case JsonToken.BEGIN_ARRAY =>
        val entry = new RawEntryBuilder
        entry.setHasDeclaredArrays()
        for ((k, v) <- moreKeys) {
          entry.addPair(k, v)
        }
        readArrayElementsInto("$_root_$", jsonReader, entry)
        entry.result()
      case JsonToken.BEGIN_OBJECT =>
        val builder = readObjectOpen(jsonReader)
        for ((k, v) <- moreKeys) {
          builder.addPair(k, v)
        }
        builder.result()
      case _ =>
        throw new ParseException("Root element can be either an object or an array, not " + jsonReader.peek())
    }
    val compressedRoot = compress(rawRoot, "")
    new HierarchicDatabase(compressedRoot.toEntry(None))
  }

  private def readArrayElementsInto(arrayKey: String, reader: JsonReader, target: RawEntryBuilder): Unit = {
    reader.beginArray()
    while (reader.hasNext) {
      reader.peek() match {
        case JsonToken.BEGIN_ARRAY =>
          readArrayElementsInto(arrayKey, reader, target)
        case JsonToken.BEGIN_OBJECT =>
          target.addArrayElement(readObject(reader))
        case JsonToken.NULL =>
          reader.nextNull()
          target.addArrayElement(new RawEntryBuilder().addPair(arrayKey, null).result())
        case JsonToken.BOOLEAN =>
          target.addArrayElement(new RawEntryBuilder().addPair(arrayKey, String.valueOf(reader.nextBoolean())).result())
        case _ =>
          target.addArrayElement(new RawEntryBuilder().addPair(arrayKey, reader.nextString()).result())
      }
    }
    reader.endArray()
  }

  private def readObjectOpen(reader: JsonReader): RawEntryBuilder = {
    reader.beginObject()
    val builder = new RawEntryBuilder
    while (reader.hasNext) {
      val key = reader.nextName()
      reader.peek() match {
        case JsonToken.BEGIN_ARRAY =>
          builder.setHasDeclaredArrays()
          readArrayElementsInto(key, reader, builder)
        case JsonToken.BEGIN_OBJECT =>
          builder.addDirect(key, readObject(reader))
        case JsonToken.NULL =>
          reader.nextNull()
          builder.addPair(key, null)
        case JsonToken.BOOLEAN =>
          builder.addPair(key, String.valueOf(reader.nextBoolean()))
        case _ =>
          builder.addPair(key, reader.nextString())
      }
    }
    reader.endObject()
    builder
  }

  private def readObject(reader: JsonReader): RawEntry = readObjectOpen(reader).result()

  private implicit class MuHashMapEx(val map: MuHashMap[String, String]) extends AnyVal {
    def updateUnique(k: String, v: String): Unit = {
      if (map.contains(k)) {
        throw new ParseException(s"Key $k already exists")
      } else map.update(k, v)
    }
  }

  private class RawEntryBuilder {
    private val myArrayChildren = new ArrayBuffer[RawEntry]()
    private val myDirectChildren = new ArrayBuffer[(String, RawEntry)]()
    private val myDirectChildrenKeys = new MuHashSet[String]
    private val myKeyValuePairs = new MuHashMap[String, String]()
    private var hasDeclaredArrays = false

    def setHasDeclaredArrays(): RawEntryBuilder = { hasDeclaredArrays = true; this }
    def addArrayElement(entry: RawEntry): RawEntryBuilder = { myArrayChildren += entry; this }
    def addPair(key: String, value: String): RawEntryBuilder = { myKeyValuePairs.updateUnique(key, value); this }
    def addDirect(key: String, entry: RawEntry): RawEntryBuilder = {
      if (myDirectChildrenKeys.contains(key)) {
        throw new ParseException(s"Key $key already exists")
      }
      myDirectChildrenKeys += key
      myDirectChildren += key -> entry
      this
    }

    def result() = RawEntry(
      hasDeclaredArrays,
      myArrayChildren.toIndexedSeq,
      myDirectChildren.toIndexedSeq,
      myKeyValuePairs.toMap
    )
  }

  private case class RawEntry(
    hasDeclaredArrays: Boolean,
    arrayChildren: IndexedSeq[RawEntry],
    directChildren: IndexedSeq[(String, RawEntry)],
    keyValuePairs: Map[String, String]
  )

  private class CompressedEntryBuilder {
    private val myArrayChildren = new ArrayBuffer[CompressedEntry]()
    private val myKeyValuePairs = new MuHashMap[String, String]()
    private var hasDeclaredArrays = false

    def setHasDeclaredArrays(): CompressedEntryBuilder = { hasDeclaredArrays = true; this }
    def addArrayElement(entry: CompressedEntry): CompressedEntryBuilder = { myArrayChildren += entry; this }
    def addPair(key: String, value: String): CompressedEntryBuilder = { myKeyValuePairs.updateUnique(key, value); this }

    def result() = CompressedEntry(hasDeclaredArrays, myArrayChildren.toIndexedSeq, myKeyValuePairs.toMap)
  }

  private case class CompressedEntry(
    hasDeclaredArrays: Boolean,
    arrayChildren: IndexedSeq[CompressedEntry],
    keyValuePairs: Map[String, String]
  ) {
    def toEntry(parent: Option[HierarchicDatabase.Entry]): HierarchicDatabase.Entry = {
      val result = new HierarchicDatabase.Entry(parent, keyValuePairs, !hasDeclaredArrays)
      val someResult = Some(result)
      arrayChildren.foreach(_.toEntry(someResult))
      result
    }
  }

  private def populate(e: RawEntry, prefix: String, builder: CompressedEntryBuilder): Unit = {
    if (e.hasDeclaredArrays) {
      builder.setHasDeclaredArrays()
    }
    for ((k, v) <- e.keyValuePairs) {
      builder.addPair(prefix + k, v)
    }
    for ((k, e) <- e.directChildren) {
      populate(e, prefix + k + ".", builder)
    }
    for (e <- e.arrayChildren) {
      builder.addArrayElement(compress(e, prefix))
    }
  }

  private def compress(e: RawEntry, prefix: String): CompressedEntry = {
    val builder = new CompressedEntryBuilder
    populate(e, prefix, builder)
    builder.result()
  }
}
