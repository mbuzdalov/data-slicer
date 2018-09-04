package ru.ifmo.ds

import java.io.PrintStream

object CLI {
  trait Module {
    def name: String
    def apply(args: Array[String]): Unit
    def printUsage(out: PrintStream): Unit

    final def usage(message: String): Nothing = CLI.usage(message)
    final def usageWrap[T](arg: T): T = CLI.usageWrap(arg)
  }

  private val modules = IndexedSeq(cli.Info, cli.Diff, cli.Compress, cli.LaTeX)

  private def usage(message: String): Nothing = {
    val err = Console.err
    err.println("Error: " + message)
    err.println(s"Usage: scala ${CLI.getClass.getName.init} <${modules.map(_.name).mkString("|")}> [args...] where")
    modules.foreach(_.printUsage(err))
    sys.exit(1)
  }

  private def usageWrap[T](arg: T): T = try arg catch {
    case th: Throwable =>
      th.printStackTrace()
      usage(th.getMessage)
  }

  def main(args: Array[String]): Unit = {
    if (args.length >= 1) {
      modules.find(_.name == args(0)) match {
        case Some(m) => usageWrap(m(args.tail))
        case None => usage("Unknown command name: '" + args(0) + "'")
      }
    } else usage("No arguments given")
  }
}
