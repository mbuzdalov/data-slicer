package ru.ifmo.ds.cli

import java.io.{File, PrintStream}

import ru.ifmo.ds.CLI
import ru.ifmo.ds.io.Json
import ru.ifmo.ds.ops.FindDifferences
import ru.ifmo.ds.ops.FindDifferences.DifferenceListener

object Diff extends CLI.Module {
  override def name: String = "diff"

  override def apply(args: Array[String]): Unit = {
    if (args.length < 3) {
      usage(args.mkString("Too few arguments:\n  '", "'\n  '", "'"))
    }
    val measure = args(0)
    val db1 = Json.loadFromFile(new File(args(1)), Map("filename" -> args(1))).filter(_.contains(measure))
    val db2 = Json.loadFromFile(new File(args(2)), Map("filename" -> args(2))).filter(_.contains(measure))
    val cats = args.drop(3)
    FindDifferences.traverse(db1, db2, cats, measure, consoleDumpingDifferenceListener)
  }

  override def printUsage(out: PrintStream): Unit = {
    out.println(s"""|  $name <measure> <filename1> <filename2> [cat-key-1] [cat-key-2] ...
                    |    Finds differences in databases given by <filename1> and <filename2>.
                    |    The comparison is done by running the Kolmogorov-Smirnov test on values of key <measure>,
                    |    interpreted as Double values. Before making the comparison, both databases are factored by
                    |    the keys [cat-key-1], [cat-key-2] etc in the specified order.
                    |    The non-matching values of category keys are also reported.""".stripMargin)
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
                                         leftValues: Seq[Double], rightValues: Seq[Double], pValue: Double): Unit = {
      if (pValue < 0.05) {
        println(s"Significant difference found for key '$key':")
        dumpSlice(slice)
        println(s"  p-value: $pValue < 0.05")
      }
    }
  }
}
