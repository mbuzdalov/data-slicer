package ru.ifmo.ds.srv

import java.io.IOException
import java.nio.charset.Charset
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters._
import ru.ifmo.ds.io.Json

object Utils {
  def gzipJson(root: Path): Unit = {
    val target = root.resolveSibling(root.getFileName.toString + ".gz")
    if (!Files.exists(target)) {
      println(s"Compressing $root to $target")
      val db = Json.fromFile(root.toFile)
      Json.writeToFile(db, target.toFile)
      println(s"Deleting $root")
      Files.delete(root)
    } else {
      println(s"Warning: both $root and $target exist, will not do anything")
    }
  }

  def writeLines(file: Path, lines: Iterable[String]): Unit = {
    Files.write(file, lines.asJava, Charset.defaultCharset())
  }

  def firstTokensOfUncommentedLines(file: Path): Set[String] = {
    def indexOrEnd(i: Int, s: String): Int = if (i < 0) s.length else i
    def firstToken(s: String): String = s.substring(0, indexOrEnd(s.indexOf(' '), s))
    Files.readAllLines(file).asScala.filterNot(_.startsWith("#")).map(firstToken).toSet
  }

  def runProcess(directory: Path, command: String*): Unit = {
    val exitCode = new ProcessBuilder(command: _*).inheritIO().directory(directory.toFile).start().waitFor()
    if (exitCode != 0) {
      throw new IOException("Exit code " + exitCode)
    }
  }
}
