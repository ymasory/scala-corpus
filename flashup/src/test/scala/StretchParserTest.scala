package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class StretchParserTest extends FunSuite {

  test("`stretch` simple case") {
    parseAll(stretch, "hello world") match {
      case Success(Stretch(List(span)), _) => expect(Span("hello world", Plain)) {span}
      case _ => fail("PARSE ERROR")
    }
  }

  test("`stretch` complex case") {
    val str = "`one` ```two``` `three`  ``four`` ``five``"
    val expected = List(
      Span("one", Mono),
      Span(" ", Plain),
      Span("two", Bold),
      Span(" ", Plain),
      Span("three", Mono),
      Span("  ", Plain),
      Span("four", Italic),
      Span(" ", Plain),
      Span("five", Italic)
    )
    parseAll(stretch, str) match {
      case Success(Stretch(lstSpans), _) => expect(lstSpans) {expected}
      case _ => fail("PARSE ERROR")
    }
  }

  test("`stretch` stays on one line") {
    val str = "`one` ``two``\n```three``` four"
    val expected = List(
      Span("one", Mono),
      Span(" ", Plain),
      Span("two", Italic)
    )
    parse(stretch, str) match {
      case Success(Stretch(lstSpans), _) => expect(lstSpans) {expected}
      case _ => fail("PARSE ERROR")
    }
  }

  test("`stretch` handles and preserves whitespace") {
    val str = " ` ` one ```two ```  `` `` "
    val expected = List(
      Span(" ", Plain),
      Span(" ", Mono),
      Span(" one ", Plain),
      Span("two ", Bold),
      Span("  ", Plain),
      Span(" ", Italic),
      Span(" ", Plain)
    )
    parse(stretch, str) match {
      case Success(Stretch(lstSpans), _) => expect(lstSpans) {expected}
      case _ => fail("PARSE ERROR")
    }
  }

  test("`stretch` handles literal ticks") {
    pending
  }
}
