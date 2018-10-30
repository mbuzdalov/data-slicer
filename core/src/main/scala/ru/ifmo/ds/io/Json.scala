package ru.ifmo.ds.io

import java.io._

import ru.ifmo.ds.Database
import ru.ifmo.ds.io.TextInputOutput._
import ru.ifmo.ds.io.json.{Reading, Writing}

object Json extends TextInputOutput {
  def fromReader(reader: Reader, moreKeys: Map[String, String] = Map.empty): Database = {
    try {
      Reading.read(reader, moreKeys)
    } catch {
      case e: IOException => throw new ParseException(e)
    }
  }

  def writeToWriter(db: Database, writer: Writer): Unit = Writing.write(db, writer, "")
}
