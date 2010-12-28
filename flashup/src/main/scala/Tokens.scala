package com.yuvimasory.flashcards

import StringUtils._

//wrapper for entire flashcard file
case class Document(dirs: List[Directive], cards: List[Flashcard]) {
  override val toString = dirs.mkString + LF + cards.mkString(LF)

  def topLeftValue: Option[String] = {
    dirs foreach { dir =>
      dir match {
        case TopLeftDirective(str) => return Some(str)
        case _ =>
      }
    }
    None
  }

  def topRightValue: Option[String] = {
    dirs foreach { dir =>
      dir match {
        case TopRightDirective(str) => return Some(str)
        case _ =>
      }
    }
    None
  }

  def useNumbering: Boolean = {
    dirs foreach { dir =>
      dir match {
        case NumberingDirective(bool) => return bool
        case _ =>
      }
    }
    false
  }
}

//wrappers for directives
sealed abstract class Directive
case class GrammarDirective(grammarVersion: Int) extends Directive() {
  override val toString = "GRAMMAR " + grammarVersion + LF
}
abstract class TopDirective(val text: String) extends Directive()
case class TopLeftDirective(override val text: String) extends TopDirective(text) {
  override val toString = "TOPLEFT " + text + LF
}
case class TopRightDirective(override val text: String) extends TopDirective(text) {
  override val toString = "TOPRIGHT " + text + LF
}
case class NumberingDirective(useNumbering: Boolean) extends Directive() {
  override val toString = "USENUMBERING " + useNumbering + LF
}

//wrappers for flashcards
case class Flashcard(front: Front, back: Back) {
  override val toString = {
    val builder = new StringBuilder
    builder append "=======================" + LF
    builder append front.toString
    builder append "=======================" + LF
    builder append back.toString
    builder append "-----------------------" + LF
    builder.toString
  }
}
case class Front(frontEls: List[FrontElement]) {
  override val toString = frontEls.mkString
}
case class FrontElement(stretch: Stretch) {
  override val toString = stretch.toString + LF
}
case class Back(backEls: List[BackElement]) {
  override val toString = backEls.mkString
}
sealed abstract class BackElement
case class CodeBlock(lines: List[Line]) extends BackElement() {
  override val toString = "{{\n" + lines.mkString + "}}\n"
}
case class Line(stretch: Stretch) extends BackElement() {
  override val toString = stretch.toString + LF
  def isEmpty: Boolean = stretch.spans.length == 0
}

//wrappers for text
final case class Stretch(spans: List[Span]) {
  override val toString = spans.mkString
}
final case class Span(text: String, dec: TextDecoration) {
  override val toString = {
    dec match {
      case Plain  =>    "[" + text + "]"
      case Mono   =>   "[[" + text + "]]"
      case Italic =>  "[[[" + text + "]]]"
      case Bold   => "[[[[" + text + "]]]]"
    }
  }
}
sealed abstract class TextDecoration
case object Plain extends TextDecoration {
  override val hashCode = 100;
}
case object Mono extends TextDecoration {
  override val hashCode = 101;
}
case object Bold extends TextDecoration {
  override val hashCode = 102;
}
case object Italic extends TextDecoration {
  override val hashCode = 103;
}
