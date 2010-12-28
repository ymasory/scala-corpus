package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class DocumentParserTest extends FunSuite {

  test("`document` accepts empty string") {
    assert(parseAll(document, Empty).successful)
  }

  test("`document` accepts directives alone, rejects space before or in the middle of directives list") {
    val doc1 =
"""
#GRAMMAR 1
#TOPRIGHT top right
""" substring(1)
    assert(parseAll(document, doc1).successful)
    assert(parseAll(document, LF + doc1).successful == false)

    val doc2 =
"""
#GRAMMAR 1

#TOPRIGHT top right
""" substring(1)
    assert(parseAll(document, doc2).successful == false)
  }

  test("`document` accepts empty lines after directives") {
    val doc =
"""
#GRAMMAR 1


""" substring(1)
    assert(parseAll(document, doc).successful)
  }

  test("`document` handles single flashcard documents") {
    val doc =
"""
* front one
back one
""" substring(1)
    parseAll(document, doc) match {
      case Success(Document(_, cards), _) => assert(cards.length === 1)
      case _ => fail("DID NOT PARSE")
    }
  }

  test("`document` handles multiple flashcards") {
    val doc =
"""
#TOPLEFT top left

* front one
back one

* front two
back two

* front three
back three
""" substring(1)
    parseAll(document, doc) match {
      case Success(Document(_, cards), _) => {
        assert(cards.length == 3)
      }
      case _ => fail("DID NOT PARSE")
    }
  }

  test("`document` handles final empty lines after flashcards") {
    val doc =
"""
#GRAMMAR 1

* front one
back one



""" substring(1)
    parseAll(document, doc) match {
      case Success(Document(_, cards), _) => assert(cards.length === 1)
      case _ => fail("DID NOT PARSE")
    }
  }
}
