package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class FlashcardParserTest extends FunSuite {

  test("`flashcard` simple case") {
    val str =
"""
* front
back
""" substring(1)

    parseAll(flashcard, str) match {
      case Success(Flashcard(a, b), _) =>
      case _ => fail("PARSE ERROR")
    }
  }

  test("`flashcard` separates Flashcards") {
    val str =
"""
* front
back

* front two
back two
""" substring(1)

    parseAll(flashcard, str) match {
      case NoSuccess(_, _) =>
      case _ => fail
    }
  }
}
