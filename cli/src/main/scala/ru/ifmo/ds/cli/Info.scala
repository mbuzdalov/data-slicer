package ru.ifmo.ds.cli

import java.io.{File, PrintStream}

import ru.ifmo.ds.io.Json
import ru.ifmo.ds.{Database, Main}

object Info extends Main.Module {
  override def name: String = "info"

  override def apply(args: Array[String]): Unit = {
    if (args.length == 0) {
      usage(s"No arguments given for command '$name'")
    } else {
      val file = new File(args(0))
      val base = Json.loadFromFile(file, Map("filename" -> file.getAbsolutePath))
      dumpBasicInfo(base)
    }
  }

  override def printUsage(out: PrintStream): Unit = {
    out.println(s"  $name <filename>: prints basic stats for the database given as <filename>")
  }

  def dumpBasicInfo(base: Database): Unit = {
    val keys = base.possibleKeys
    val entries = base.entries
    println(s"${keys.size} keys, ${entries.size} entries")
    for (key <- keys.toIndexedSeq.sorted) {
      val values = base.valuesUnderKey(key)
      if (values.isEmpty) {
        println(s"  $key => no values (which is strange)")
      } else {
        val builder = new StringBuilder("  ").append(key).append(" => ")
        val iterator = values.toIndexedSeq.sorted.iterator

        def composeString(addedCount: Int): Unit = {
          if (iterator.hasNext) {
            val next = iterator.next()
            if (builder.length + 2 + next.length <= 80) {
              if (addedCount > 0) {
                builder.append(", ")
              }
              builder.append('"').append(next).append('"')
              composeString(addedCount + 1)
            } else {
              builder.append("... (").append(values.size).append(" value(s))")
            }
          }
        }
        composeString(0)
        println(builder.result())
      }
    }

    val keyProfile = entries.groupBy(e => keys.filter(e.contains))
    println(s"${keyProfile.size} key profile(s)")
    for ((k, v) <- keyProfile) {
      val keys = k.toIndexedSeq.sorted
      println(keys.mkString("  {", ", ", "}"))
      println(s"    ${v.size} values, including:")
      val example = v.head
      for (key <- keys) {
        println(s"      $key => ${example(key)}")
      }
    }
  }
}
