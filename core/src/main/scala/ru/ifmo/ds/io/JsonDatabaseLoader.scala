package ru.ifmo.ds.io

import java.io._

import scala.collection.mutable.{ArrayBuffer, HashMap => MuHashMap}

import com.google.gson.stream.{JsonReader, JsonToken}
import ru.ifmo.ds.RawDatabase

object JsonDatabaseLoader {
  def load(contents: String): RawDatabase = {
    val reader = new StringReader(contents)
    val result = load(reader)
    reader.close()
    result
  }

  def load(file: File): RawDatabase = {
    val reader = new FileReader(file)
    val result = load(reader)
    reader.close()
    result
  }

  def load(reader: Reader): RawDatabase = {
    try {
      val jsonReader = new JsonReader(reader)
      val context = new LoadContext(jsonReader)
      context.load(None)

      val leaves = context.leaves
      val nodes = context.nodes
      val roots = nodes.filter(_.isRoot)

      val keysBuilder = Set.newBuilder[String]
      roots.foreach(_.collectKeys(keysBuilder += _))

      new RawDatabase {
        override val possibleKeys: Set[String] = keysBuilder.result()
        override val entries: Seq[RawDatabase.Entry] = leaves.map(new JsonEntry(_))

        override def valuesUnderKey(key: String): Set[String] = {
          val builder = Set.newBuilder[String]
          roots.foreach(_.collectValuesFor(key, builder += _))
          builder.result()
        }
      }
    } catch {
      case e: IOException => throw new ParseException(e)
    }
  }

  private class LoadContext(reader: JsonReader) {
    private[this] val leavesBuilder = IndexedSeq.newBuilder[HierarchyLookup]
    private[this] val nodesBuilder = IndexedSeq.newBuilder[HierarchyLookup]

    def leaves: IndexedSeq[HierarchyLookup] = leavesBuilder.result()
    def nodes:  IndexedSeq[HierarchyLookup] = nodesBuilder.result()

    def loadSideObject(lookup: HierarchyLookup, prefix: String): Unit = {
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

    def load(parent: Option[HierarchyLookup]): Unit = {
      reader.peek() match {
        case JsonToken.BEGIN_ARRAY =>
          reader.beginArray()
          while (reader.hasNext) {
            load(parent)
          }
          reader.endArray()
        case JsonToken.BEGIN_OBJECT =>
          reader.beginObject()
          val currentLookup = new HierarchyLookup(parent)
          nodesBuilder += currentLookup
          val someCurrentLookup = Some(currentLookup)
          var isLeaf = true
          while (reader.hasNext) {
            val key = reader.nextName()
            if (currentLookup.get(key).isDefined) {
              throw new ParseException(s"The key '$key' is used in an enclosing object")
            }
            reader.peek() match {
              case JsonToken.BEGIN_ARRAY  => isLeaf = false; load(someCurrentLookup)
              case JsonToken.BEGIN_OBJECT => loadSideObject(currentLookup, key)
              case JsonToken.NULL         => reader.nextNull(); currentLookup.put(key, null)
              case JsonToken.BOOLEAN      => currentLookup.put(key, String.valueOf(reader.nextBoolean()))
              case _                      => currentLookup.put(key, reader.nextString())
            }
          }
          if (isLeaf) {
            leavesBuilder += currentLookup
          }
          reader.endObject()
        case _ =>
          throw new ParseException(s"The root element should be either an object or an array")
      }
    }
  }

  private class HierarchyLookup(parent: Option[HierarchyLookup]) {
    parent.foreach(_.myChildren += this)

    private[this] val myKeys = new MuHashMap[String, String]()

    private val myChildren = new ArrayBuffer[HierarchyLookup]

    def isRoot: Boolean = parent.isEmpty
    def put(key: String, value: String): Unit = myKeys.update(key, value)
    def get(key: String): Option[String] = myKeys.get(key).orElse(parent.flatMap(_.get(key)))

    def collectKeys(collector: String => Unit): Unit = {
      myKeys.keySet.foreach(collector)
      myChildren.foreach(_.collectKeys(collector))
    }

    def collectValuesFor(key: String, collector: String => Unit): Unit = {
      if (myKeys.contains(key)) {
        collector(myKeys(key))
      } else {
        myChildren.foreach(_.collectValuesFor(key, collector))
      }
    }
  }

  private class JsonEntry(val lookup: HierarchyLookup) extends RawDatabase.Entry {
    override def contains(key: String): Boolean = lookup.get(key).isDefined
    override def get(key: String): Option[String] = lookup.get(key)
    override def apply(key: String): String = lookup.get(key).getOrElse(throw new IllegalArgumentException(s"No such key '$key'"))
  }

  class ParseException(message: String, cause: Throwable) extends RuntimeException {
    def this(message: String) = this(message, null)
    def this(cause: Throwable) = this(null, cause)
  }
}
