package ru.ifmo.ds.util

import scala.annotation.tailrec

object OrderingForStringWithNumbers {
  /**
    * This ordering will compare strings by splitting them internally into sequences of either
    * non-numeric or numeric ('0' to '9') characters. Non-numeric sequences are compared lexicographically,
    * numeric sequences are compared as numbers. No other distinctions are made.
    *
    * This ordering will put "0.5" before "0.10", however, it will correctly sort IPv4 strings.
    */
  final val NoSpecialDotTreatment: Ordering[String] = new OrderingForStringWithNumbers('0')

  /**
    * This ordering will compare strings by splitting them internally into sequences of either
    * non-numeric or numeric ('0' to '9') characters. Non-numeric sequences are compared lexicographically,
    * numeric sequences are compared as numbers. However, numeric sequences coming immediately after
    * the dot symbol are also treated lexicographically, as if they follow the decimal point.
    *
    * This ordering will put "0.10" before "0.5", so it is suitable for sorting floating-point values.
    * However, the "1.25e-4" will come after "1.256".
    */
  final val SpecialDotTreatment: Ordering[String] = new OrderingForStringWithNumbers('.')
}

class OrderingForStringWithNumbers(specialSymbol: Char) extends Ordering[String] {
  private[this] def isNumeric(char: Char): Boolean = char >= '0' && char <= '9'

  @tailrec
  private[this] def lex(x: String, y: String, ix: Int, iy: Int, endX: Int): Int = {
    if (ix == endX) 0 else {
      val currX = x.charAt(ix)
      val currY = y.charAt(iy)
      if (currX != currY) {
        currX.toInt - currY.toInt
      } else lex(x, y, ix + 1, iy + 1, endX)
    }
  }

  private[this] def consumeZeros(x: String, ix: Int): Int = {
    var rv = ix
    while (rv < x.length && x.charAt(rv) == '0') {
      rv += 1
    }
    rv
  }

  private[this] def consumeNumbers(x: String, ix: Int): Int = {
    var rv = ix
    while (rv < x.length && isNumeric(x.charAt(rv))) {
      rv += 1
    }
    rv
  }

  @tailrec
  private[this] def run(x: String, y: String, ix: Int, iy: Int, shouldNegateComparison: Boolean): Int = {
    if (x.length == ix) {
      if (y.length == iy) 0 else -1
    } else if (y.length == iy) 1 else {
      val currX = x.charAt(ix)
      val currY = y.charAt(iy)
      val isNumX = isNumeric(currX)
      val isNumY = isNumeric(currY)

      if (isNumX && isNumY && (ix == 0 && iy == 0 || x.charAt(ix - 1) != specialSymbol)) {
        val ixNonZero = consumeZeros(x, ix)
        val iyNonZero = consumeZeros(y, iy)

        // we negate either if we inherit this, or if minus comes at the beginning
        val multiple = if (shouldNegateComparison) -1 else 1

        // collect numbers and compare them
        val endX = consumeNumbers(x, ixNonZero)
        val endY = consumeNumbers(y, iyNonZero)

        if (endX - ixNonZero == endY - iyNonZero) {
          // collected numbers have equal length, now comes lexicography
          val lexResult = lex(x, y, ixNonZero, iyNonZero, endX)
          if (lexResult == 0) {
            run(x, y, endX, endY, shouldNegateComparison)
          } else lexResult * multiple
        } else if (endX - ixNonZero < endY - iyNonZero) -multiple else multiple
      } else if (currX != currY) {
        // we are in lexicographic mode
        // this can be the after-dot mode
        val multiple = if (isNumX && isNumY && shouldNegateComparison) -1 else 1
        (currX.toInt - currY.toInt) * multiple
      } else {
        val newShouldNegate = if (shouldNegateComparison) {
          isNumX && isNumY || currX == specialSymbol && x.charAt(ix - 1) != specialSymbol
        } else ix == 0 && currX == '-'
        run(x, y, ix + 1, iy + 1, newShouldNegate)
      }
    }
  }

  override def compare(x: String, y: String): Int = {
    if (x == null) {
      if (y == null) 0 else -1
    } else if (y == null) 1 else run(x, y, 0, 0, shouldNegateComparison = false)
  }
}
