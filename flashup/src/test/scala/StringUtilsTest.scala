package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import StringUtils._

class StringUtilsTest extends FunSuite {

  test("rnTrim") {
    expect("hello") {
      rnTrim("\n\nhello\r\n")
    }
    expect("println(\"hello world\")\nexit(0)") {
      rnTrim("\nprintln(\"hello world\")\nexit(0)\n")
    }
    expect("") {
      rnTrim("\r\n")
    }
  }

  test("leftTrim") {
    expect("hello\r\n") {
      leftTrim("\n \f\nhello\r\n")
    }
    expect("") {
      leftTrim("\r\n\f")
    }
  }

  test("rightTrim") {
    expect("\r\nhello") {
      rightTrim("\r\nhello\r\n \f")
    }
    expect("") {
      rightTrim("\r\n\f")
    }
  }
}
