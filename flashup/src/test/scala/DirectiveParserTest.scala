package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

class DirectiveParserTest extends FunSuite {

  test("`grammarDirective extraction") {
    parseAll(grammarDirective, "#GRAMMAR 0\n") match {
      case Success(GrammarDirective(0), _) =>
      case _ => fail
    }
    parseAll(grammarDirective, "#GRAMMAR 112\n") match {
      case Success(GrammarDirective(112), _) =>
      case _ => fail
    }
    parseAll(grammarDirective, "#GRAMMAR \t1 \t\n") match {
      case Success(GrammarDirective(1), _) =>
      case _ => fail
    }
  }

  test("`grammarDirective rejects invalid directives") {
    assert(parseAll(grammarDirective, "#GRAMMAR \n").successful === false)
    assert(parseAll(grammarDirective, "#GRAMMAR \n fd \r\n fd").successful === false)
    assert(parseAll(grammarDirective, "#GRAMMAR -1\n").successful === false)
    assert(parseAll(grammarDirective, "#GRAMMAR f1\n").successful === false)
    assert(parseAll(grammarDirective, " #GRAMMAR 1\n").successful === false)
    assert(parseAll(grammarDirective, "# GRAMMAR 1\n").successful === false)
    assert(parseAll(grammarDirective, "#grammar 1\n").successful === false)
  }

  test("`numberingDirective extraction") {
    parseAll(numberingDirective, "#NUMBERS true\n") match {
      case Success(NumberingDirective(true), _) =>
      case _ => fail
    }
    parseAll(numberingDirective, "#NUMBERS false\n") match {
      case Success(NumberingDirective(false), _) =>
      case _ => fail
    }
    parseAll(numberingDirective, "#NUMBERS   \t\t true  \t\n") match {
      case Success(NumberingDirective(true), _) =>
      case _ => fail
    }
  }

  test("`numberingDirective rejects invalid directives") {
    assert(parseAll(numberingDirective, "#NUMBERS \n").successful === false)
    assert(parseAll(numberingDirective, "#NUMBERS \n true\n").successful === false)
    assert(parseAll(numberingDirective, "# NUMBERS true\n").successful === false)
    assert(parseAll(numberingDirective, "#NUMBERS False\n").successful === false)
    assert(parseAll(numberingDirective, "#numbers false\n").successful === false)
  }

  test("`topDirective` extraction") {
    List((topLeftDirective -> "TOPLEFT"), (topRightDirective -> "TOPRIGHT")) foreach { pair =>
      val parser: Parser[TopDirective] = pair._1
      val cmd: String = pair._2
      parseAll(parser, "#" + cmd + " hello\n") match {
        case Success(dir, _) => assert(dir.text === "hello")
        case _ => fail
      }
      parseAll(parser, "#" + cmd + " hello \t world \n") match {
        case Success(dir, _) => assert(dir.text === "hello \t world")
        case _ => fail
      }
     }
   }

  test("`topDirectives` reject invalid directives") {
    List((topLeftDirective, "topleft"), (topRightDirective, "topright")) foreach { pair =>
      val parser: Parser[TopDirective] = pair._1
      val str: String = pair._2
      assert(parseAll(parser, "#" + str + " hello\n").successful === false)
    }

    List((topLeftDirective, "TOPLEFT"), (topRightDirective, "TOPRIGHT")) foreach { pair =>
      val parser: Parser[Any] = pair._1
      val str: String = pair._2
      assert(parseAll(parser, "#" + str  + "  \n").successful === false)
      assert(parseAll(parser, "#" + str + " hello\n fd \r\n fd").successful === false)
      assert(parseAll(parser, " #" + str + " hello\n").successful === false)
      assert(parseAll(parser, "# " + str + " hello\n").successful === false)
    }
  }
}
