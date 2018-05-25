package ru.ifmo.ds.io

import java.io._

import com.google.gson.stream.{JsonReader, JsonToken}
import ru.ifmo.ds.{Database, HierarchicDatabase}

object Json {
  def loadFromString(contents: String, moreKeys: Map[String, String] = Map.empty): Database = {
    val reader = new StringReader(contents)
    val result = loadFromReader(reader, moreKeys)
    reader.close()
    result
  }

  def loadFromFile(file: File, moreKeys: Map[String, String] = Map.empty): Database = {
    val reader = new FileReader(file)
    val result = loadFromReader(reader, moreKeys)
    reader.close()
    result
  }

  def loadFromReader(reader: Reader, moreKeys: Map[String, String] = Map.empty): Database = {
    try {
      val jsonReader = new JsonReader(reader)
      val context = new LoadContext(jsonReader)
      val globalParent = new HierarchicDatabase.ImmutableEntry(None, moreKeys)
      context.load(globalParent, None)
      new HierarchicDatabase(globalParent)
    } catch {
      case e: IOException => throw new ParseException(e)
    }
  }

  private class LoadContext(reader: JsonReader) {
    def loadSideObject(lookup: HierarchicDatabase.MutableEntry, prefix: String): Unit = {
      reader.beginObject()
      while (reader.hasNext) {
        val key = prefix + "." + reader.nextName()
        reader.peek() match {
          case JsonToken.BEGIN_ARRAY  => throw new ParseException("Array is not allowed in side objects")
          case JsonToken.BEGIN_OBJECT => loadSideObject(lookup, key)
          case JsonToken.NULL         => reader.nextNull(); lookup.put(key, null)
          case JsonToken.BOOLEAN      => lookup.put(key, String.valueOf(reader.nextBoolean()))
          case _                      => lookup.put(key, reader.nextString())
        }
      }
      reader.endObject()
    }

    def load(parent: HierarchicDatabase.Entry, key: Option[String]): Unit = {
      val someParent = Some(parent)
      reader.peek() match {
        case JsonToken.BEGIN_ARRAY =>
          reader.beginArray()
          while (reader.hasNext) {
            load(parent, key)
          }
          reader.endArray()
        case JsonToken.BEGIN_OBJECT =>
          reader.beginObject()
          val currentLookup = new HierarchicDatabase.MutableEntry(someParent)
          while (reader.hasNext) {
            val key = reader.nextName()
            if (currentLookup.contains(key)) {
              throw new ParseException(s"The key '$key' is already used in the same or in an enclosing object")
            }
            reader.peek() match {
              case JsonToken.BEGIN_ARRAY  => load(currentLookup, Some(key))
              case JsonToken.BEGIN_OBJECT => loadSideObject(currentLookup, key)
              case JsonToken.NULL         => reader.nextNull(); currentLookup.put(key, null)
              case JsonToken.BOOLEAN      => currentLookup.put(key, String.valueOf(reader.nextBoolean()))
              case _                      => currentLookup.put(key, reader.nextString())
            }
          }
          reader.endObject()
        case otherToken => key match {
          case None => throw new ParseException(s"The root element should be either an object or an array, found '$otherToken'")
          case Some(k) =>
            // A scheme to support { "measurements": [1.42345, 1.345346, 1.435346] }
            val currentLookup = new HierarchicDatabase.MutableEntry(someParent)
            otherToken match {
              case JsonToken.NULL    => reader.nextNull(); currentLookup.put(k, null)
              case JsonToken.BOOLEAN => currentLookup.put(k, String.valueOf(reader.nextBoolean()))
              case _                 => currentLookup.put(k, reader.nextString())
            }
        }
      }
    }
  }

  class ParseException(message: String, cause: Throwable) extends RuntimeException {
    def this(message: String) = this(message, null)
    def this(cause: Throwable) = this(null, cause)
  }
}
