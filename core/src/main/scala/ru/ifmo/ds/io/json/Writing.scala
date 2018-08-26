package ru.ifmo.ds.io.json

import java.io.Writer

import com.google.gson.stream.JsonWriter

import ru.ifmo.ds.Database

object Writing {
  private[this] object State extends Enumeration {
    val TopLevel, InsideArray, InsideObject = Value
  }

  private[this] def findNeutral(keys: Set[String]): String = {
    if (keys.contains("_")) {
      Stream.from(0).map("_" + _).filterNot(keys.contains).head
    } else "_"
  }

  def write(db: Database, writer: Writer, indent: String): Unit = {
    val neutral = findNeutral(db.possibleKeys)
    val json = new JsonWriter(writer)
    json.setIndent(indent)

    def everyoneStartsWith(keys: Iterable[String], p: String) = keys.forall(_.startsWith(p))

    def firstDottedToken(key: String): String = {
      val idx = key.indexOf('.')
      if (idx < 0) key else key.substring(0, idx)
    }

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

    def dropKeyPrefix(keys: Set[String], prefixLength: Int, e: Database.Entry): Database.Entry = {
      Database.entry(keys.flatMap(k => e.get(k).map(v => k -> v)).map(p => (p._1.substring(prefixLength), p._2)).toMap)
    }

    def write(keys0: Set[String], entries: Seq[Database.Entry], state: State.Value): Unit = {
      val keys = keys0.filter(k => entries.exists(_.contains(k)))
      val ldp = longestDottedPrefix(keys)
      if (ldp.nonEmpty && keys.size > 1) {
        val newKeys = keys.map(_.substring(ldp.length + 1))
        val newEntries = entries.map(e => dropKeyPrefix(keys, ldp.length + 1, e))
        state match {
          case State.TopLevel | State.InsideArray =>
            json.beginObject()
            json.name(ldp)
            json.beginObject()
            write(newKeys, newEntries, State.InsideObject)
            json.endObject()
            json.endObject()
          case State.InsideObject =>
            json.name(ldp)
            json.beginObject()
            write(newKeys, newEntries, State.InsideObject)
            json.endObject()
        }
      } else {
        val nDifferentValues = keys.map(k => k -> entries.iterator.map(_.get(k)).toSet.size).toMap
        val (singularKeys, nonSingularKeys) = keys.partition(k => nDifferentValues(k) == 1)
        val shallInitObject = (singularKeys.nonEmpty || nonSingularKeys.size == 1) && state != State.InsideObject
        val newState = if (shallInitObject) {
          json.beginObject()
          State.InsideObject
        } else state

        // It can happen that non-singular keys have a huge common prefix, which is shared with at least one of singular keys.
        // This can be done in a more compact way than using a purely separate encoding.
        if (nonSingularKeys.nonEmpty && longestDottedPrefix(nonSingularKeys).nonEmpty && {
          val first = firstDottedToken(nonSingularKeys.head) + "."
          singularKeys.exists(_.startsWith(first))
        }) {
          val first = firstDottedToken(nonSingularKeys.head) + "."
          val (mainstreamSingular, extraSingular) = singularKeys.partition(_.startsWith(first))
          // we are inside an object, so this hack is safe
          write(extraSingular, Seq(entries.head), State.InsideObject)
          write(mainstreamSingular ++ nonSingularKeys, entries, State.InsideObject)
        } else {
          if (singularKeys.nonEmpty) {
            val example = entries.head
            if (singularKeys.size == 1) {
              val theKey = singularKeys.head
              json.name(theKey).value(example(theKey))
            } else {
              singularKeys.groupBy(firstDottedToken).foreach(p => write(p._2, Seq(example), State.InsideObject))
            }
          }
          if (nonSingularKeys.nonEmpty) {
            if (nonSingularKeys.size == 1) {
              // we are inside the object, so we can safely write an array directly
              val theKey = nonSingularKeys.head
              json.name(theKey).beginArray()
              for (e <- entries) {
                json.value(e(theKey))
              }
              json.endArray()
            } else {
              newState match {
                case State.TopLevel => json.beginArray()
                case State.InsideObject => json.name(neutral).beginArray()
                case State.InsideArray =>
              }

              val splitKey = nonSingularKeys.minBy(nDifferentValues)
              if (entries.exists(!_.contains(splitKey)) && entries.exists(_.contains(splitKey))) {
                // This requires a separate treatment unfortunately.
                val (withSome, withNone) = entries.partition(_.contains(splitKey))
                write(nonSingularKeys, withSome, State.InsideArray)
                write(nonSingularKeys, withNone, State.InsideArray)
              } else {
                entries.groupBy(_.get(splitKey)).foreach(g => write(nonSingularKeys, g._2, State.InsideArray))
              }

              if (newState != State.InsideArray) {
                json.endArray()
              }
            }
          }
        }

        if (shallInitObject) {
          json.endObject()
        }
      }
    }

    val entries = db.entries
    if (entries.isEmpty) {
      json.beginArray()
      json.endArray()
    } else if (db.possibleKeys.isEmpty) {
      json.beginObject()
      json.endObject()
    } else {
      write(db.possibleKeys, entries, State.TopLevel)
    }
    json.flush()
  }
}
