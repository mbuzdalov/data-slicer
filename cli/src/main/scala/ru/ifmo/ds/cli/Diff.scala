package ru.ifmo.ds.cli

import java.io.{File, PrintStream}

import spire.math.Rational

import ru.ifmo.ds.io.Json
import ru.ifmo.ds.ops.FindDifferences
import ru.ifmo.ds.ops.FindDifferences.DifferenceListener
import ru.ifmo.ds.stat.{RankSumResultJoiner, TestResult}
import ru.ifmo.ds.{CLI, Database}

object Diff extends CLI.Module {
  override def name: String = "diff"

  override def apply(args: Array[String]): Unit = {
    if (args.length < 1) {
      usage(args.mkString("Too few arguments:\n  '", "'\n  '", "'"))
    }
    val measure = args(0)
    val opposeOpt = new CommandLineOption("--oppose", 3, 3)
    val filesOpt = new CommandLineOption("--files", 1)
    val catsOpt = new CommandLineOption("--cats", 1)
    val fileNameKeyOpt = new CommandLineOption("--filename-key", 1)
    val pValueOpt = new CommandLineOption("-p", 1, 1)
    val reportSingleOpt = new CommandLineOption("--report-single", 0, 0)
    val reportCatsOpt = new CommandLineOption("--report-cats", 0, Int.MaxValue)
    val filenameOpt = new CommandLineOption("", 2, 2, true, {
      val errorMsg = "At least one file name is explicitly specified outside any command-line option"
      opposeOpt.disable(errorMsg)
      filesOpt.disable(errorMsg)
    })

    CommandLineOption.submit(Seq(
      opposeOpt, filesOpt, catsOpt, pValueOpt, filenameOpt, reportSingleOpt, reportCatsOpt
    ), args.tail :_*)

    val pValue = pValueOpt.resultOrElse(IndexedSeq("0.05")).head.toDouble
    val nakedFileNames = filenameOpt.resultOrElse(IndexedSeq.empty)
    val fileNameKey = fileNameKeyOpt.resultOrElse(IndexedSeq("filename")).head

    def build(filename: String) = Json.fromFile(new File(filename), Map(fileNameKey -> filename)).filter(_.contains(measure))

    val consoleListener = new ConsoleListener(
      p = pValue,
      reportSingle = reportSingleOpt.isPresent,
      reportCats = if (reportCatsOpt.isPresent) reportCatsOpt.result().toSet else Set(" "))

    if (nakedFileNames.isEmpty) {
      val opposeArgs = opposeOpt.result()
      val files = filesOpt.result()
      val db = Database.merge(files.map(build) :_*)
      FindDifferences.traverse(db, opposeArgs(0), _.contains(opposeArgs(1)), _.contains(opposeArgs(2)), catsOpt.result(),
                               measure, consoleListener)
    } else {
      val db1 = build(nakedFileNames(0))
      val db2 = build(nakedFileNames(1))
      FindDifferences.traverse(db1, db2, catsOpt.result(), measure, consoleListener)
    }
  }

  override def printUsage(out: PrintStream): Unit = {
    out.println(s"""|  $name <measure-key> <DB defining args> [-p <p-value>] [--cats [cat-key-1] [cat-key-2] ...]
                    |                                         [--filename-key <fn-key>]
                    |                                         [--report-single]
                    |                                         [--report-cats [cat-key-1] [cat-key-2] ...]
                    |    Finds differences in databases given by <DB defining args>. While loading databases,
                    |    the file name is added to each database under the key <fn-key> ("filename" by default).
                    |    The comparison is done by running the Kolmogorov-Smirnov test on values under <measure-key>,
                    |    interpreted as Double values. Before making the comparison, both databases are factored by
                    |    the keys [cat-key-1], [cat-key-2] etc in the specified order.
                    |    The p-value threshold used to determine whether the result is significant is specified by -p,
                    |    or otherwise the value of 0.05 is used.
                    |    The non-matching values of category keys are also reported.
                    |
                    |    If --report-single option is specified, every single comparison below the threshold is reported.
                    |    If --report-cats option is specified, for slices consisting only of the given keys ([cat-key-1],
                    |    [cat-key-2] etc) the RankSumResultJoiner.join procedure will be run on the Kolmogorov-Smirnov
                    |    outcomes, and if the result is below the threshold, this slice will be reported.
                    |
                    |    The <DB defining args> are either:
                    |      <filename 1> <filename 2>
                    |        Compare two given databases.
                    |      --oppose <key> <left-value> <right-value> --files <file1> [file2] ...
                    |        Merges all files from the --files section into a single database,
                    |        then takes all entries with <key> having <left-value> to the left-hand side,
                    |        and all entries with <key> having <right-value> to the right-hand side.""".stripMargin)
  }

  private[this] class ConsoleListener(p: Double, reportSingle: Boolean, reportCats: Set[String]) extends DifferenceListener {
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
                                         result: TestResult[Rational]): Unit = {
      if (reportSingle && result.p < p) {
        println(s"Significant difference found for key '$key':")
        dumpSlice(slice)
        println(leftValues.sorted.mkString("  left values: [", ", ", "]"))
        println(rightValues.sorted.mkString("  right values: [", ", ", "]"))
        println(s"  p-value: ${result.p} < $p")
      }
    }

    override def sliceStatistics(slice: Map[String, Option[String]], key: String,
                                 statistics: Seq[TestResult[Rational]]): Unit = {
      if (statistics.nonEmpty && slice.keySet == reportCats) {
        val first = statistics.head
        def sameSize(s1: TestResult[Rational], s2: TestResult[Rational]) = {
          s1.firstSampleSize == s2.firstSampleSize && s1.secondSampleSize == s2.secondSampleSize
        }
        statistics.find(v => !sameSize(first, v)) match {
          case Some(otherSize) =>
            println(s"Different sample sizes found: (${
              first.firstSampleSize}, ${first.secondSampleSize
            }) and (${
              otherSize.firstSampleSize}, ${otherSize.secondSampleSize
            })")
            dumpSlice(slice)
          case None =>
            // all sizes are equal
            val p = RankSumResultJoiner.join(statistics)
            if (p < this.p) {
              println(s"Significant difference found in multiple comparisons for key '$key':")
              dumpSlice(slice)
              println(s"  p-value: $p < ${this.p}")
            }
        }
      }
    }
  }
}
