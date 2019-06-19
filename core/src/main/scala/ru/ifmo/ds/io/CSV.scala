package ru.ifmo.ds.io
import java.io.{BufferedReader, Reader, Writer}
import java.util.StringTokenizer

import ru.ifmo.ds.Database

object CSV extends TextInputOutput {
  override def fromReader(reader: Reader, moreKeys: Map[String, String]): Database = {
    def splitByComma(str: String): Array[String] = {
      val tok = new StringTokenizer(str, ",")
      Array.fill(tok.countTokens())(tok.nextToken())
    }

    def readEntryAndContinue(reader: BufferedReader, keys: Array[String], target: Database.Entry => Unit): Unit = {
      val line = reader.readLine()
      if (line != null) {
        val tokens = splitByComma(line)
        if (tokens.length == keys.length) {
          target(Database.entry(keys.lazyZip(tokens).toMap))
        }
        readEntryAndContinue(reader, keys, target)
      }
    }

    val lines = new BufferedReader(reader)
    val keys = splitByComma(lines.readLine())

    val entries = IndexedSeq.newBuilder[Database.Entry]
    readEntryAndContinue(lines, keys, entries += _)
    Database(entries.result() :_*).withMoreKeys(moreKeys)
  }

  override def writeToWriter(db: Database, writer: Writer): Unit = {
    val keys = db.possibleKeys
    writer.append(keys.mkString("", ",", "\n"))
    for (e <- db.entries) {
      writer.append(keys.map(k => e.get(k).getOrElse("")).mkString("", ",", "\n"))
    }
    writer.flush()
  }
}
