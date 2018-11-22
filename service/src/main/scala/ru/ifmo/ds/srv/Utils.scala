package ru.ifmo.ds.srv

import java.nio.file.{Files, Path}

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
}
