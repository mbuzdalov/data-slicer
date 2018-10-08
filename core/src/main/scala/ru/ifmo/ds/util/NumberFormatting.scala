package ru.ifmo.ds.util

import java.util.Locale

object NumberFormatting {
  def format(value: Double, precision: Int)(format: (Int, String, Int) => String): String = {
    val formatString = s"%.0${precision}e"
    val str = formatString.formatLocal(Locale.US, value)
    val point0 = str.indexOf('.')
    val e = str.indexOf('e')
    val point = if (point0 == -1) e else point0
    format(str.substring(0, point).toInt, if (point == e) "" else str.substring(point + 1, e), str.substring(e + 1).toInt)
  }

  def scientificFormat(head: Int, tail: String, pow: Int): String = s"$$$head.$tail \\cdot 10^{$pow}$$"
  def computerFormat(head: Int, tail: String, pow: Int): String = s"$head.${tail}e$pow"
  def shortFormat(head: Int, tail: String, pow: Int): String = if (pow >= 0) {
    s"$$$head.$tail^{+$pow}$$"
  } else {
    s"$$$head.$tail^{$pow}$$"
  }
}
