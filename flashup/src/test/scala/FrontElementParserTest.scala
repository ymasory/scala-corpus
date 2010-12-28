package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class FrontElementParserTest extends FunSuite {

  test("`frontElement` simple case") {
    parseAll(frontElement, "* hello world\n") match {
      case Success(FrontElement(Stretch(List(text))), _) => expect(Span("hello world", Plain)) {text}
      case _ => fail("PARSE ERROR")
    }
  }

  test("`frontElement` stays on one line") {
    parse(frontElement, "* hello world\ngoodbye") match {
      case Success(FrontElement(Stretch(List(text))), _) => expect(Span("hello world", Plain)) {text}
      case _ => fail("PARSE ERROR")
    }
  }

  test("`frontElement` accepts two stars on a line") {
    parseAll(frontElement, "* hello * world\n") match {
      case Success(FrontElement(Stretch(List(text))), _) => expect(Span("hello * world", Plain)) {text}
      case _ => fail("PARSE ERROR")
    }
  }

  test("`frontElement` ignores leading and trailing whitespace") {
    parseAll(frontElement, "* \t  hello world  \t  \n") match {
      case Success(FrontElement(Stretch(List(text))), _) => expect(Span("hello world", Plain)) {text}
      case _ => fail("PARSE ERROR")
    }
  }

  test("`frontElement` doesn't require the conventional space after star") {
    parseAll(frontElement, "*hello world\n") match {
      case Success(FrontElement(Stretch(List(text))), _) => expect(Span("hello world", Plain)) {text}
      case _ => fail("PARSE ERROR")
    }
  }

  test("`frontElement` demands a star start the line") {
    parseAll(frontElement, " * hello world\n") match {
      case NoSuccess(_, _) =>
      case _ => fail
    }
  }
}
