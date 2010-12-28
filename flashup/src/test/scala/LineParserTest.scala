package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class LineParserTest extends FunSuite {

  test("`line` simple case") {
    parseAll(line, "hello world\n") match {
      case Success(Line(Stretch(List(text))), _) => expect(Span("hello world", Plain)) {text}
      case _ => fail("PARSE ERROR")
    }
  }

  test("`line` can't start with a star") {
    parseAll(line, "* next front\n") match {
      case NoSuccess(_, _) =>
      case _ => fail
    }
  }

  test("`line` stays on one line") {
    parse(line, "hello world\ngoodbye") match {
      case Success(Line(Stretch(List(text))), _) => expect(Span("hello world", Plain)) {text}
      case _ => fail("PARSE ERROR")
    }
  }

  test("`line` ignores trailing (but preserves leading) whitespace") {
    parseAll(line, " \t  hello world  \t  \n") match {
      case Success(Line(Stretch(List(text))), _) => expect(Span(" \t  hello world", Plain)) {text}
      case _ => fail("PARSE ERROR")
    }
  }

  test("iss10 - `line` allows lines that include Ticks") {
    val str =
"""
hello `world`
""" substring(1)
    parseAll(line, str) match {
      case Success(_, _) =>
      case _ => fail("PARSE ERROR")
    }
  }
}
