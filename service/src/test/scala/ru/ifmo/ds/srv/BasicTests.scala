package ru.ifmo.ds.srv

import org.scalatest.{FlatSpec, Matchers}

class BasicTests extends FlatSpec with Matchers {
  "A call to Main with no arguments" should "fail" in {
    assertThrows[RuntimeException] {
      Main.main(new Array[String](0))
    }
  }
}
