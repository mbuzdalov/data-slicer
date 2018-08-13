package ru.ifmo.ds.cli

import java.io.{File, PrintStream}

import ru.ifmo.ds.CLI
import ru.ifmo.ds.io.Json

object Compress extends CLI.Module {
  override def name: String = "compress"

  override def apply(args: Array[String]): Unit = {
    if (args.length > 2 || args.length == 0) {
      usage("Wrong number of arguments: expected either 1 or 2 arguments")
    } else {
      val source = args(0)
      val target = if (args.length == 2) args(1) else args(0)
      Json.writeToFile(Json.fromFile(new File(source)), new File(target))
    }
  }

  override def printUsage(out: PrintStream): Unit = {
    out.println(s"  $name <source-file> [target-file]: compresses the database from 'source-file'.")
    out.println("       Stores it in 'target-file' if it exists, overwrites 'source-file' otherwise. ")
  }
}
