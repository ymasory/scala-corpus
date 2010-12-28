package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class BackParserTest extends FunSuite {

  test("`back` rejects empty string") {
    parseAll(back, Empty) match {
      case NoSuccess(_, _) =>
      case Success(_, _) => fail
    }
  }

  test("iss11 - `back` eliminates leading and trailing empty lines") {
    val str = 
"""

hello

world

""" substring(1)
    parseAll(back, str) match {
      case Success(Back(List(one, two, three)), _) =>
      case _ => fail("PARSE ERROR")
    }
  }

  test("`back` doesn't over parse") {
    val str = 
"""
hello

* next front
""" substring(1)

    parseAll(back, str) match {
      case NoSuccess(_, _) =>
      case _ => fail
    }
  }

  test("`back` complex case") {
    val str =
"""

`
hello
world
`


hello
goodbye
`
hello
world
`

""" substring(1)
    parseAll(back, str) match {
      case Success(Back(List(one, two, three, four, five, six)), _) =>
      case _ => fail("PARSE ERROR")
    }
  }
}
