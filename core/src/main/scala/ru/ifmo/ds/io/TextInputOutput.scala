package ru.ifmo.ds.io

import java.io._
import java.nio.file.Path
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scala.util.{Failure, Success, Try}

import ru.ifmo.ds.Database

trait TextInputOutput {
  def fromReader(reader: Reader, moreKeys: Map[String, String] = Map.empty): Database
  def writeToWriter(db: Database, writer: Writer): Unit

  def fromString(contents: String, moreKeys: Map[String, String] = Map.empty): Database = {
    val reader = new StringReader(contents)
    val result = fromReader(reader, moreKeys)
    reader.close()
    result
  }

  def fromPath(path: Path, moreKeys: Map[String, String] = Map.empty): Database = fromFile(path.toFile, moreKeys)

  def fromFile(file: File, moreKeys: Map[String, String] = Map.empty): Database = {
    var stream: FileInputStream = null
    var zip: GZIPInputStream = null
    var reader: InputStreamReader = null
    try {
      // first try to read it as a zip file
      stream = new FileInputStream(file)
      zip = new GZIPInputStream(stream)
      reader = new InputStreamReader(zip)
      fromReader(reader, moreKeys)
    } catch {
      case zipEx: IOException =>
        tryCloseSuppressed(reader, zipEx)
        tryCloseSuppressed(zip, zipEx)
        tryCloseSuppressed(stream, zipEx)

        var directReader: FileReader = null
        try {
          directReader = new FileReader(file)
          fromReader(directReader, moreKeys)
        } catch {
          case directEx: IOException =>
            directEx.addSuppressed(zipEx)
            throw directEx
        } finally {
          tryClose(directReader)
        }
    } finally {
      tryClose(reader)
      tryClose(zip)
      tryClose(stream)
    }
  }

  def toString(db: Database): String = {
    val sw = new StringWriter()
    writeToWriter(db, sw)
    sw.toString
  }

  def writeToPath(db: Database, path: Path): Unit = writeToFile(db, path.toFile)

  def writeToFile(db: Database, file: File): Unit = {
    if (file.getName.endsWith(".gz")) {
      val stream = new FileOutputStream(file)
      val gzip = new GZIPOutputStream(stream)
      val writer = new OutputStreamWriter(gzip)
      writeToWriter(db, writer)
      writer.close()
      gzip.close()
      stream.close()
    } else {
      val sw = new FileWriter(file)
      writeToWriter(db, sw)
      sw.close()
    }
  }


  private[this] def tryClose(what: Closeable): Unit = {
    try {
      if (what != null) what.close()
    } catch {
      case _: Throwable =>
    }
  }

  private[this] def tryCloseSuppressed(what: Closeable, toAdd: Exception): Unit = {
    try {
      if (what != null) what.close()
    } catch {
      case th: Throwable => toAdd.addSuppressed(th)
    }
  }
}

object TextInputOutput {
  class ParseException(message: String, cause: Throwable) extends RuntimeException(message, cause) {
    def this(message: String) = this(message, null)
    def this(cause: Throwable) = this(null, cause)
  }

  private implicit class TryOps[+T](val t: Try[T]) extends AnyVal {
    def tryMore[U >: T](fun: => U): Try[U] = t match {
      case s@Success(_) => s
      case f@Failure(_) => try {
        Success(fun)
      } catch {
        case th: Throwable =>
          f.exception.addSuppressed(th)
          f
      }
    }
  }

  def fromFile(file: File, moreKeys: Map[String, String] = Map.empty): Database = {
    Try(Json.fromFile(file, moreKeys))
      .tryMore(CSV.fromFile(file, moreKeys))
      .get
  }
}
