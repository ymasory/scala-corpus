package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class ItalicSpanParserTest extends FunSuite {

  test("`italicSpan` simple case") {
    parseAll(italicSpan, "``hello world``") match {
      case Success(span, _) => expect(Span("hello world", Italic)) {span}
      case _ => fail("PARSE FAILED")
    }
  }

  test("`italicSpan` preserves whitespace") {
    parseAll(italicSpan, "``  hello world  ``") match {
      case Success(span, _) => expect(Span("  hello world  ", Italic)) {span}
      case _ => fail("PARSE FAILED")
    }
  }

  test("`italicSpan` stays on one line") {
    parse(italicSpan, "``hello\nworld``") match {
      case NoSuccess(_, _) =>
      case Success(_, _) => fail
    }
  }

  test("`italicSpan` accepts tick literals") {
    pending
    parse(italicSpan, """``hello \`world\```""") match {
      case Success(span, _) => expect(Span("hello `world`", Italic)) {span}
      case _ => fail("PARSE FAILED")
    }
  }
}
