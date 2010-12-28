package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class BackElementParserTest extends FunSuite {

  test("`backElement` simple case with Line") {
    parseAll(backElement, "hello world\n") match {
      case Success(Line(_), _) =>
      case _ => fail("PARSE ERROR")
    }
  }

  test("`backElement` simple case with CodeBlock") {
    val str =
"""
`
hello
world
`
""" substring(1)
    parseAll(backElement, str) match {
      case Success(CodeBlock(_), _) =>
      case _ => fail("PARSE ERROR")
    }
  }
}
