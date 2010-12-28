package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class MonoSpanParserTest extends FunSuite {

  test("`monoSpan` simple case") {
    parseAll(monoSpan, "`hello world`") match {
      case Success(span, _) => expect(Span("hello world", Mono)) {span}
      case _ => fail("PARSE FAILED")
    }
  }

  test("`monoSpan` preserves whitespace") {
    parseAll(monoSpan, "`  hello world  `") match {
      case Success(span, _) => expect(Span("  hello world  ", Mono)) {span}
      case _ => fail("PARSE FAILED")
    }
  }

  test("`monoSpan` stays on one line") {
    parse(monoSpan, "`hello\nworld`") match {
      case NoSuccess(_, _) =>
      case Success(_, _) => fail
    }
  }

  test("`monoSpan` accepts tick literals") {
    pending
    parse(monoSpan, """`hello \`world\``""") match {
      case Success(span, _) => expect(Span("hello `world`", Mono)) {span}
      case _ => fail("PARSE FAILED")
    }
  }
}
