package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class BoldSpanParserTest extends FunSuite {

  test("`boldSpan` simple case") {
    parseAll(boldSpan, "```hello world```") match {
      case Success(span, _) => expect(Span("hello world", Bold)) {span}
      case _ => fail("PARSE FAILED")
    }
  }

  test("`boldSpan` preserves whitespace") {
    parseAll(boldSpan, "```  hello world  ```") match {
      case Success(span, _) => expect(Span("  hello world  ", Bold)) {span}
      case _ => fail("PARSE FAILED")
    }
  }

  test("`boldSpan` stays on one line") {
    parse(boldSpan, "```hello\nworld```") match {
      case NoSuccess(_, _) =>
      case Success(_, _) => fail
    }
  }

  test("`boldSpan` accepts tick literals") {
    pending
    parse(boldSpan, """```hello \`world\````""") match {
      case Success(span, _) => expect(Span("hello `world`", Bold)) {span}
      case _ => fail("PARSE FAILED")
    }
  }
}
