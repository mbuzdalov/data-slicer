package ru.ifmo.ds.util

import org.scalatest.{FlatSpec, Matchers}

import OrderingForStringWithNumbers._

class OrderingForStringWithNumbersTests extends FlatSpec with Matchers {
  private def runRemainingChecks(ord: Ordering[String]): Unit = {
    it should "do well with nulls" in {
      ord.compare(null, null) shouldBe 0
      ord.compare(null, "42") should be < 0
      ord.compare("42", null) should be > 0
    }

    it should "do well with empty strings" in {
      ord.compare(null, "") should be < 0
      ord.compare("", null) should be > 0
      ord.compare("", "") shouldBe 0
      ord.compare("", "42") should be < 0
      ord.compare("42", "") should be > 0
    }

    it should "do well with non-negative numbers" in {
      ord.compare("0", "1") should be < 0
      ord.compare("1", "0") should be > 0
      ord.compare("42", "42") shouldBe 0
      ord.compare("042", "42") shouldBe 0
      ord.compare("9", "10") should be < 0
      ord.compare("10", "9") should be > 0
    }

    it should "do well with negative numbers" in {
      ord.compare("0", "-1") should be > 0
      ord.compare("-1", "0") should be < 0
      ord.compare("-42", "-42") shouldBe 0
      ord.compare("-042", "-42") shouldBe 0
      ord.compare("-9", "-10") should be > 0
      ord.compare("-10", "-9") should be < 0
    }

    it should "do well with text strings" in {
      ord.compare("abcd", "abce") should be < 0
      ord.compare("abcd", "abd") should be < 0
    }

    it should "do well with interleaved strings" in {
      ord.compare("0abcd", "1abce") should be < 0
      ord.compare("9abcd", "10abce") should be < 0
      ord.compare("1abcd", "0abce") should be > 0
      ord.compare("10abcd", "9abce") should be > 0
      ord.compare("a99", "a100") should be < 0
      ord.compare("a100", "a50") should be > 0
      ord.compare("a-1", "a1") should be < 0
    }

    it should "not miss minuses with hyphens" in {
      ord.compare("a-9", "a-8") should be > 0
      ord.compare("test-42-1", "test-42-2") should be < 0
      ord.compare("-9a", "-8a") should be < 0
    }
  }

  "NoSpecialDotTreatment" should "do well with IP-like sequences" in {
    NoSpecialDotTreatment.compare("0.1.2.3", "1.99.78.26") should be < 0
    NoSpecialDotTreatment.compare("0.10", "0.9") should be > 0
  }

  runRemainingChecks(NoSpecialDotTreatment)

  "SpecialDotTreatment" should "do well with simple floating-point numbers" in {
    SpecialDotTreatment.compare("0.10", "0.9") should be < 0
    SpecialDotTreatment.compare("-0.10", "-0.9") should be > 0
    SpecialDotTreatment.compare("-0..10", "-0..9") should be < 0
  }

  runRemainingChecks(SpecialDotTreatment)
}
