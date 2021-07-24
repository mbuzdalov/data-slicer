package ru.ifmo.ds.io

import java.io.{BufferedReader, IOException, Reader, Writer}
import java.util.StringTokenizer

import ru.ifmo.ds.Database

object OpenFoamPostProcessing extends TextInputOutput {
  private def readTokens(str: String): Array[String] = {
    val tokenizer = new StringTokenizer(str, ", \t()")
    Array.fill(tokenizer.countTokens())(tokenizer.nextToken())
  }

  override def fromReader(reader: Reader, moreKeys: Map[String, String]): Database = {
    val bufferedReader = new BufferedReader(reader)
    var lastSharpLine = "!#"
    var line = bufferedReader.readLine().trim
    while (line != null && line(0) == '#') {
      lastSharpLine = line
      line = bufferedReader.readLine()
    }
    if (lastSharpLine(0) != '#')
      throw new IOException("Unrecognized input format: there is no header including #-line with field names")
    if (line == null)
      throw new IOException("Unrecognized input format: no lines follow the last line with #")
    val lastSharpTokens = readTokens(lastSharpLine).filter(_ != "#")
    val rows = IndexedSeq.newBuilder[Array[String]]

    while (line != null && line != "") {
      val firstRow = readTokens(line.trim)
      if (firstRow.length != lastSharpTokens.length)
        throw new IOException("Unrecognized input format: first data line does not match the header")
      rows += firstRow
      line = bufferedReader.readLine()
    }

    Database(rows.result() map { row => Database.entry(lastSharpTokens.lazyZip(row).toMap) } :_*).withMoreKeys(moreKeys)
  }

  override def writeToWriter(db: Database, writer: Writer): Unit =
    throw new UnsupportedOperationException("No writing is yet supported")
}
