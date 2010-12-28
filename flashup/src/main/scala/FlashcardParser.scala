package com.yuvimasory.flashcards

import java.io.{BufferedReader, File, FileReader, InputStream, Reader}

import scala.util.parsing.combinator._

import StringUtils._

object FlashcardParser {

  def parseDoc(in: Reader): Option[Document] = {
    import ComponentParsers._
    parseAll(document, in) match {
      case Success(doc, _) => Some(doc)
      case _ => None
    }
  }

  def parseDoc(in: File): Option[Document] = {
    parseDoc(new BufferedReader(new FileReader(in)))
  }

  private[flashcards] object ComponentParsers extends RegexParsers {

    //some handy literal Parser
    lazy val Hash     : Parser[String] = "#"
    lazy val Star     : Parser[String] = "*"
    lazy val Tick     : Parser[String] = "`"
    lazy val True     : Parser[String] = "true"
    lazy val False    : Parser[String] = "false"
    lazy val TopLeft  : Parser[String] = "TOPLEFT"
    lazy val TopRight : Parser[String] = "TOPRIGHT"
    lazy val Grammar  : Parser[String] = "GRAMMAR"
    lazy val Numbering: Parser[String] = "NUMBERS"

    //some handy constructed Parsers
    lazy val endl: Parser[String] = LF
    lazy val blankInlineChar: Parser[String] = """[\f\t ]""".r
    lazy val inlineChar: Parser[String] = """[^\r\n]""".r
    lazy val inlineTicklessChar: Parser[String] = """[^\r\n`]""".r
    lazy val inlineStarlessChar: Parser[String] = """[^\*\r\n]""".r
    lazy val inlineTicklessStarlessChar: Parser[String] = """[^\*\r\n`]""".r

    //now let's build our flashcard grammar, top-down, starting with the root Document element
    lazy val document: Parser[Document] = (directive*) ~ (endl*) ~ (flashcard*) ~ (endl*) ^^ {
      case lstDir ~ _ ~ lstCard ~ _ => Document(lstDir, lstCard)
    }

    lazy val directive: Parser[Directive] = grammarDirective | topLeftDirective | topRightDirective | numberingDirective

    lazy val grammarDirective: Parser[GrammarDirective] =
      Hash ~> Grammar ~> (blankInlineChar+) ~>
      (("""\d""".r)+) <~
      (blankInlineChar*) <~ endl ^^ {
        case digits => GrammarDirective(digits.mkString.toInt)
      }

    lazy val numberingDirective: Parser[NumberingDirective] =
      Hash ~> Numbering ~> (blankInlineChar+) ~>
      (True | False) <~
      (blankInlineChar*) <~ endl ^^ {
        case bool => NumberingDirective(java.lang.Boolean.parseBoolean(bool))
      }

    lazy val (topLeftDirective, topRightDirective) = {
      def topDirective(isLeft: Boolean): Parser[TopDirective] = {
        Hash ~> {if (isLeft) TopLeft else TopRight} ~> (blankInlineChar*) ~>
        """.*\S""".r <~
        (blankInlineChar*) <~ endl ^^ {
          case chars => {
            val str = chars.mkString
            if (isLeft) TopLeftDirective(str)
            else TopRightDirective(str)
          }
        }
      }
      (topDirective(true), topDirective(false))
    }

    lazy val flashcard: Parser[Flashcard] = front ~ back ^^ {
      case f ~ b => Flashcard(f, b)
    }

    lazy val front: Parser[Front] = (frontElement+) ^^ {
      case fEls => Front(fEls)
    }

    lazy val backElement: Parser[BackElement] = codeBlock | line

    lazy val codeBlock: Parser[CodeBlock] = Tick ~> endl ~> (ticklessLine*) <~ Tick <~ endl ^^ {
      case lstLines => CodeBlock(lstLines)
    }

    lazy val stretch: Parser[Stretch] = ((boldSpan | italicSpan | monoSpan | plainSpan)*) ^^ {
      case lstSpans => Stretch(lstSpans)
    }

    lazy val plainSpan: Parser[Span] = (inlineTicklessChar+) ^^ {
      case chars =>  Span(chars.mkString, Plain)
    }

    lazy val monoSpan: Parser[Span] = Tick ~> (inlineTicklessChar+) <~ Tick ^^ {
      case chars => Span(chars.mkString, Mono)
    }

    lazy val italicSpan: Parser[Span] = repN(2, Tick) ~> (inlineTicklessChar+) <~ repN(2, Tick) ^^ {
      case chars => Span(chars.mkString, Italic)
    }

    lazy val boldSpan: Parser[Span] = repN(3, Tick) ~> (inlineTicklessChar+) <~ repN(3, Tick) ^^ {
      case chars => Span(chars.mkString, Bold)
    }













    lazy val frontElement: Parser[FrontElement] = Star ~> (blankInlineChar*) ~>
      """.*\S""".r <~
      (blankInlineChar*) <~ endl ^^ {
        case chars => {
          val str = chars.mkString
          parseAll(stretch, str) match {
            case Success(a_stretch, _) => FrontElement(a_stretch)
            //iss8
            case NoSuccess(_, _) => FrontElement(Stretch(List(Span(str, Plain))))
          }
        }
    }

    lazy val (ticklessLine, line) = {
      def line(allowTicks: Boolean): Parser[Line] =
        (endl ^^ {x => Line(Stretch(Nil))}) |
        ({if(allowTicks) inlineStarlessChar else inlineTicklessStarlessChar} ~ ({if(allowTicks) inlineChar else inlineTicklessChar}*) <~ endl) ^^ {
        case head ~ tail => {
          val str = rightTrim(head.mkString + tail.mkString)
          parseAll(stretch, str) match {
            case Success(a_stretch, _) => Line(a_stretch)
            //iss8
            case NoSuccess(_, _) => Line(Stretch(List(Span(str, Plain))))
          }
        }
      }
      (line(false), line(true))
    }


    lazy val back: Parser[Back] = (endl*) ~> (backElement+) ^^ {
      case bEls => {
        var lastGood = -1
        (0 until bEls.length) foreach { i =>
          val bEl = bEls(i)
          bEl match {
            case l: Line if l.isEmpty =>
            case _ => lastGood = i
          }
        }
        if (lastGood < 0)
          Back(bEls)
        else
          Back(bEls.slice(0, lastGood + 1))
      }
    }








    /** We'll take care of whitespace ourselves, thank you very much. */
    override val skipWhitespace = false
  }
}

