package ru.ifmo.ds.util

import org.scalatest.{FlatSpec, Matchers}

class NumberFormattingTests extends FlatSpec with Matchers {
  import NumberFormatting._

  "Scientific formatting" should "format integers as expected" in {
    format(1, 0)(scientificFormat) shouldBe "$1. \\cdot 10^{0}$"
    format(1, 1)(scientificFormat) shouldBe "$1.0 \\cdot 10^{0}$"
    format(1, 2)(scientificFormat) shouldBe "$1.00 \\cdot 10^{0}$"

    format(10, 0)(scientificFormat) shouldBe "$1. \\cdot 10^{1}$"
    format(10, 1)(scientificFormat) shouldBe "$1.0 \\cdot 10^{1}$"
    format(10, 2)(scientificFormat) shouldBe "$1.00 \\cdot 10^{1}$"

    format(10000, 0)(scientificFormat) shouldBe "$1. \\cdot 10^{4}$"
    format(10000, 1)(scientificFormat) shouldBe "$1.0 \\cdot 10^{4}$"
    format(10000, 2)(scientificFormat) shouldBe "$1.00 \\cdot 10^{4}$"

    format(90, 0)(scientificFormat) shouldBe "$9. \\cdot 10^{1}$"
    format(90, 1)(scientificFormat) shouldBe "$9.0 \\cdot 10^{1}$"
    format(90, 2)(scientificFormat) shouldBe "$9.00 \\cdot 10^{1}$"
  }

  it should "format few floating-point numbers as expected" in {
    format(math.Pi, 1)(scientificFormat) shouldBe "$3.1 \\cdot 10^{0}$"
    format(math.Pi, 2)(scientificFormat) shouldBe "$3.14 \\cdot 10^{0}$"
    format(math.Pi, 3)(scientificFormat) shouldBe "$3.142 \\cdot 10^{0}$"
    format(math.Pi, 4)(scientificFormat) shouldBe "$3.1416 \\cdot 10^{0}$"
    format(math.Pi, 5)(scientificFormat) shouldBe "$3.14159 \\cdot 10^{0}$"
    format(math.Pi, 6)(scientificFormat) shouldBe "$3.141593 \\cdot 10^{0}$"

    format(0.12345, 3)(scientificFormat) shouldBe "$1.235 \\cdot 10^{-1}$"
  }

  it should "format even numbers close to 10^t from below as expected" in {
    format(9.999999999999, 1)(scientificFormat) shouldBe "$1.0 \\cdot 10^{1}$"
    format(9.999999999999, 2)(scientificFormat) shouldBe "$1.00 \\cdot 10^{1}$"
    format(9.999999999999, 3)(scientificFormat) shouldBe "$1.000 \\cdot 10^{1}$"
    format(9.999999999999, 4)(scientificFormat) shouldBe "$1.0000 \\cdot 10^{1}$"
    format(9.999999999999, 5)(scientificFormat) shouldBe "$1.00000 \\cdot 10^{1}$"
    format(9.999999999999, 6)(scientificFormat) shouldBe "$1.000000 \\cdot 10^{1}$"
    format(9.999999999999, 7)(scientificFormat) shouldBe "$1.0000000 \\cdot 10^{1}$"
  }

  "Short formatting" should "format floating-point numbers as expected" in {
    format(math.Pi, 1)(shortFormat) shouldBe "$3.1^{+0}$"
    format(math.Pi, 2)(shortFormat) shouldBe "$3.14^{+0}$"
    format(math.Pi, 3)(shortFormat) shouldBe "$3.142^{+0}$"
    format(math.Pi, 4)(shortFormat) shouldBe "$3.1416^{+0}$"
    format(math.Pi, 5)(shortFormat) shouldBe "$3.14159^{+0}$"
    format(math.Pi, 6)(shortFormat) shouldBe "$3.141593^{+0}$"

    format(0.12345, 3)(shortFormat) shouldBe "$1.235^{-1}$"
    format(42.1215512, 2)(shortFormat) shouldBe "$4.21^{+1}$"
  }
}
