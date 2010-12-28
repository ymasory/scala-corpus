package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class FrontParserTest extends FunSuite {

  test("`front` simple case") {
    val str = "* one\n"
    parseAll(front, str) match {
      case Success(Front(List(one)), _) =>
      case _ => fail("PARSE ERROR")
    }
  }

  test("`front` with multiple elements") {
    val str = "* one\n* two\n*three\n"
    parseAll(front, str) match {
      case Success(Front(List(one, two, three)), _) =>
      case _ => fail("PARSE ERROR")
    }
  }

  test("`front` rejects lines between front elements") {
    val str = "* one\n\n*two\n"
    parse(front, str) match {
      case Success(Front(List(one)), _) =>
      case _ => fail("PARSE ERROR")
    }
  }

  test("`front` doesn't over-parse") {
    val str =
"""
* one
* two

"""
    parseAll(front, str) match {
      case NoSuccess(_, _) =>
      case _ => fail
    }
  }
}
