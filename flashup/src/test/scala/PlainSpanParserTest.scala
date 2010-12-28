package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class PlainSpanParserTest extends FunSuite {

  test("`plainSpan` simple case") {
    parseAll(plainSpan, "hello world") match {
      case Success(span, _) => expect(Span("hello world", Plain)) {span}
      case _ => fail("PARSE FAILED")
    }
  }

  test("`plainSpan` preserves whitespace") {
    parseAll(plainSpan, "  hello world  ") match {
      case Success(span, _) => expect(Span("  hello world  ", Plain)) {span}
      case _ => fail("PARSE FAILED")
    }
  }

  test("`plainSpan` stays on one line") {
    parse(plainSpan, "hello\nworld") match {
      case Success(span, _) => expect(Span("hello", Plain)) {span}
      case _ => fail("PARSE FAILED")
    }
  }

  test("`plainSpan` accepts tick literals") {
    pending
    parse(plainSpan, """hello \`world\`""") match {
      case Success(span, _) => expect(Span("hello `world`", Plain)) {span}
      case _ => fail("PARSE FAILED")
    }
  }
}
