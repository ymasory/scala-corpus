package com.yuvimasory.flashcards

import org.scalatest.FunSuite

import FlashcardParser._
import FlashcardParser.ComponentParsers._

import StringUtils._

class CodeBlockParserTest extends FunSuite {

  test("`codeBlock` simple case") {
    val str =
"""
`
hello
`
""" substring(1)
    parseAll(codeBlock, str) match {
      case Success(
        CodeBlock(
          List(
            Line(
              Stretch(
                List(
                  Span(
                    hello, Plain)))))), _) => expect("hello") {hello}
      case _ => fail("PARSE ERROR")
    }
  }

  test("`codeBlock` preserves empty lines") {
    val str =
"""
`

hello

world

`
""" substring(1)
    parseAll(codeBlock, str) match {
      case Success(CodeBlock(List(one, two, three, four, five)), _) => 
      case _ => fail("PARSE ERROR")
    }
  }

  test("`codeBlock` doesn't overshoot") {
    val str =
"""
`
hello
world
`

`
goodbye
world
`
""" substring(1)
    parse(codeBlock, str) match {
      case Success(CodeBlock(List(one, two)), _) =>
      case _ => fail("PARSE ERROR")
    }
  }

  test("`codeBlock` insists tics are on their own line") {
    val str1 =
"""
`f
hello
`
""" substring(1)
    val str2 =
"""
`
hello
`f
""" substring(1)
    parseAll(codeBlock, str1) match {
      case NoSuccess(_, _) =>
      case _ => fail
    }
    parseAll(codeBlock, str2) match {
      case NoSuccess(_, _) =>
      case _ => fail
    }
  }
}
