package ru.ifmo.ds.io.json

import java.io.Writer

import com.google.gson.stream.JsonWriter

import ru.ifmo.ds.Database

object Writing {
  private[this] def findNeutral(keys: Set[String]): String = {
    if (keys.contains("_")) {
      Stream.from(0).map("_" + _).filterNot(keys.contains).head
    } else "_"
  }

  def write(db: Database, writer: Writer, indent: String): Unit = {
    val neutral = findNeutral(db.possibleKeys)
    val json = new JsonWriter(writer)
    json.setIndent(indent)
    import scala.collection.mutable

    def writePlain(map: Map[String, String]): Unit = map.foreach(p => json.name(p._1).value(p._2))

    def everyoneStartsWith(keys: Iterable[String], p: String) = keys.forall(_.startsWith(p))

    def longestDottedPrefix(keys: Iterable[String]): String = {
      val headKey = keys.head
      def go(prev: Int): Int = {
        val nextDot = headKey.indexOf('.', prev + 1)
        if (nextDot < 0) prev else {
          val newPrefix = headKey.substring(0, nextDot + 1) // including the dot
          if (everyoneStartsWith(keys, newPrefix)) {
            go(nextDot)
          } else prev
        }
      }
      val maxPrefix = go(-1)
      if (maxPrefix <= 0) "" else headKey.substring(0, maxPrefix)
    }

    def writeCompact(map: Map[String, String]): Unit = {
      // we are in an Object already
      if (map.size == 1) {
        writePlain(map)
      } else if (map.nonEmpty) {
        // first try to tear of longest prefix ending with a dot
        def firstDotSegment(s: String) = {
          val dot = s.indexOf('.')
          if (dot == -1) s else s.substring(0, dot)
        }

        val maxDotPrefix = longestDottedPrefix(map.keys)
        if (maxDotPrefix.nonEmpty) {
          json.name(maxDotPrefix) // not including the dot
          json.beginObject()
          writeCompact(map.map(p => p._1.substring(maxDotPrefix.length + 1) -> p._2))
          json.endObject()
        } else {
          map.groupBy(p => firstDotSegment(p._1)).foreach(z => writeCompact(z._2))
        }
      }
    }

    def wrapSingle(keys: Set[String], entry: Database.Entry): Unit = {
      json.beginObject()
      writeCompact(keys.flatMap(k => entry.get(k).map(v => k -> v)).toMap)
      json.endObject()
    }

    def create(keys: Set[String], entries: Seq[Database.Entry], hasParent: Boolean): Unit = {
      require(entries.nonEmpty)
      if (entries.size == 1) wrapSingle(keys, entries.head) else {
        val possibleValues = keys.map(k => (k, new mutable.HashSet[Option[String]]())).toMap
        entries.foreach(e => possibleValues.foreach(p => p._2 += e.get(p._1)))
        val (singleValued, multipleValued) = possibleValues.partition(_._2.size == 1)
        if (multipleValued.isEmpty) {
          wrapSingle(keys, entries.head)
        } else if (multipleValued.size == 1) {
          val sliceKey = multipleValued.head._1
          val shouldMakeNewBraces = singleValued.nonEmpty || !hasParent
          if (shouldMakeNewBraces) {
            json.beginObject()
            writeCompact(singleValued.flatMap(p => p._2.head.map(v => p._1 -> v)))
          }
          json.name(sliceKey).beginArray()
          for (e <- entries) {
            json.value(e(sliceKey))
          }
          json.endArray()
          if (shouldMakeNewBraces) {
            json.endObject()
          }
        } else {
          val minMultiValued = multipleValued.minBy(_._2.size)
          assert(minMultiValued._2.size > 1)
          val sliceKey = minMultiValued._1
          val shouldMakeNewBraces = singleValued.nonEmpty || !hasParent
          if (shouldMakeNewBraces) {
            json.beginObject()
            writeCompact(singleValued.flatMap(p => p._2.head.map(v => p._1 -> v)))
            json.name(neutral).beginArray()
          }
          val restOfKeys = keys.diff(singleValued.keySet)
          for ((_, slice) <- entries.groupBy(_.get(sliceKey))) {
            create(restOfKeys, slice, hasParent = true)
          }
          if (shouldMakeNewBraces) {
            json.endArray()
            json.endObject()
          }
        }
      }
    }

    val entries = db.entries
    if (entries.isEmpty) {
      json.beginArray()
      json.endArray()
    } else {
      create(db.possibleKeys, entries, hasParent = false)
    }
    json.flush()
  }
}
