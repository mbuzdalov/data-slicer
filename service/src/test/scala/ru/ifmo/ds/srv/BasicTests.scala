package ru.ifmo.ds.srv

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BasicTests extends AnyFlatSpec with Matchers {
  "A call to Main with no arguments" should "fail" in {
    assertThrows[RuntimeException] {
      Main.main(new Array[String](0))
    }
  }
}
