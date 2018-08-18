package ru.ifmo.ds.cli

import java.io.{File, PrintStream}

import ru.ifmo.ds.{CLI, Database}
import ru.ifmo.ds.io.Json
import ru.ifmo.ds.ops.FindDifferences
import ru.ifmo.ds.ops.FindDifferences.DifferenceListener
import ru.ifmo.ds.stat.ApproximateKolmogorovSmirnov

object Diff extends CLI.Module {
  override def name: String = "diff"

  override def apply(args: Array[String]): Unit = {
    if (args.length < 3) {
      usage(args.mkString("Too few arguments:\n  '", "'\n  '", "'"))
    }
    val measure = args(0)
    if (args(1) == "--oppose") {
      if (args.length < 8) {
        usage(args.mkString("Too few arguments for --oppose:\n  '", "'\n  '", "'"))
      } else {
        val oppositionKey = args(2)
        val oppositionLeftValue = args(3)
        val oppositionRightValue = args(4)
        val startFiles = args.indexOf("--files")
        val startCats = args.indexOf("--cats")
        if (startFiles < 5) {
          usage("No --files found for the --oppose option")
        }
        val (files, cats) = if (startCats == -1) {
          (args.drop(startFiles + 1), Array.empty[String])
        } else if (startFiles < startCats) {
          (args.slice(startFiles + 1, startCats), args.drop(startCats + 1))
        } else {
          (args.drop(startFiles + 1), args.slice(startCats + 1, startFiles))
        }
        val db = Database.merge(files.map(f => Json.fromFile(new File(f), Map("filename" -> f))) :_*)
        FindDifferences.traverse(db, oppositionKey, Some(oppositionLeftValue), Some(oppositionRightValue), cats,
                                 measure, consoleDumpingDifferenceListener)
      }
    } else {
      val db1 = Json.fromFile(new File(args(1)), Map("filename" -> args(1))).filter(_.contains(measure))
      val db2 = Json.fromFile(new File(args(2)), Map("filename" -> args(2))).filter(_.contains(measure))
      val cats = args.drop(3)
      FindDifferences.traverse(db1, db2, cats, measure, consoleDumpingDifferenceListener)
    }
  }

  override def printUsage(out: PrintStream): Unit = {
    out.println(s"""|  $name <measure-key> <filename1> <filename2> [cat-key-1] [cat-key-2] ...
                    |    Finds differences in databases given by <filename1> and <filename2>.
                    |    The comparison is done by running the Kolmogorov-Smirnov test on values under <measure-key>,
                    |    interpreted as Double values. Before making the comparison, both databases are factored by
                    |    the keys [cat-key-1], [cat-key-2] etc in the specified order.
                    |    The non-matching values of category keys are also reported.
                    |  $name <measure-key> --oppose <key> <left-value> <right-value>
                    |                     --files <file1> [file2] ...
                    |                    [--cats [cat-key-1] [cat-key-2]]
                    |    Does the same as the above, except that first all the given files are merged into one database,
                    |    and then the left database is taken as its slice with <key> equal to <left-value>,
                    |    and the right database is similarly its slice with <key> equal to <right-value>.""".stripMargin)
  }

  final val consoleDumpingDifferenceListener: DifferenceListener = new DifferenceListener {
    private[this] def dumpSlice(slice: Map[String, Option[String]]): Unit = {
      println("  For the following slice:")
      for ((k, v) <- slice) {
        println(s"    '$k' => $v")
      }
    }

    private[this] def dumpExclusive(key: String, name: String, set: Set[Option[String]]): Unit = {
      if (set.nonEmpty) {
        println(s"  ${set.size} values of '$key' exist only in the $name database:")
        for (v <- set) {
          println(s"    '$v'")
        }
      }
    }

    override def keyValuesDoNotMatch(slice: Map[String, Option[String]], key: String,
                                     onlyLeft: Set[Option[String]], onlyRight: Set[Option[String]]): Unit = {
      println(s"Mismatching values found for key '$key':")
      dumpSlice(slice)
      dumpExclusive(key, "left", onlyLeft)
      dumpExclusive(key, "right", onlyRight)
    }

    override def kolmogorovSmirnovFailure(slice: Map[String, Option[String]], key: String,
                                          leftValues: Seq[String], rightValues: Seq[String], exception: Throwable): Unit = {
      println(s"Error while running the Kolmogorov-Smirnov test for key '$key':")
      dumpSlice(slice)
      println(s"  ${exception.getClass.getName}: ${exception.getMessage}")
    }

    override def kolmogorovSmirnovResult(slice: Map[String, Option[String]], key: String,
                                         leftValues: Seq[Double], rightValues: Seq[Double],
                                         result: ApproximateKolmogorovSmirnov.Result): Unit = {
      if (result.p < 0.05) {
        println(s"Significant difference found for key '$key':")
        dumpSlice(slice)
        println(leftValues.sorted.mkString("  left values: [", ", ", "]"))
        println(rightValues.sorted.mkString("  right values: [", ", ", "]"))
        println(s"  p-value: ${result.p} < 0.05")
      }
    }
  }
}
