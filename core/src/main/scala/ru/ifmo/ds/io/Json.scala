package ru.ifmo.ds.io

import java.io._

import ru.ifmo.ds.Database
import ru.ifmo.ds.io.json.{Reading, Writing}

object Json {
  class ParseException(message: String, cause: Throwable) extends RuntimeException {
    def this(message: String) = this(message, null)
    def this(cause: Throwable) = this(null, cause)
  }

  def fromString(contents: String, moreKeys: Map[String, String] = Map.empty): Database = {
    val reader = new StringReader(contents)
    val result = fromReader(reader, moreKeys)
    reader.close()
    result
  }

  def fromFile(file: File, moreKeys: Map[String, String] = Map.empty): Database = {
    val reader = new FileReader(file)
    val result = fromReader(reader, moreKeys)
    reader.close()
    result
  }

  def fromReader(reader: Reader, moreKeys: Map[String, String] = Map.empty): Database = {
    try {
      Reading.read(reader, moreKeys)
    } catch {
      case e: IOException => throw new ParseException(e)
    }
  }

  def toString(db: Database): String = {
    val sw = new StringWriter()
    writeToWriter(db, sw)
    sw.toString
  }

  def writeToFile(db: Database, file: File): Unit = {
    val sw = new FileWriter(file)
    writeToWriter(db, sw)
    sw.close()
  }

  def writeToWriter(db: Database, writer: Writer): Unit = Writing.write(db, writer, "")
}
