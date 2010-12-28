package com.yuvimasory.flashcards

import java.util.regex.Pattern

object StringUtils {

  val LF = "\n"
  val Empty = ""
  val NullStr = "\u0000"
  
  /** Return the substring of `in` with initial
    * and final \r and \n characters removed. */
  def rnTrim(in: String): String = {
    var goOn = true
    var firstGood = -1
    var lastGood = -1
    (0 until in.length) foreach { i =>
      val c: Char = in(i)
      if (goOn) {
        if (c != '\r' && c != '\n') {
          firstGood = i
          goOn = false
        }
      }
    }
    goOn = true
    (in.length - 1 to 0 by -1) foreach { i =>
      val c: Char = in(i)
      if (goOn) {
        if (c != '\r' && c != '\n') {
          lastGood = i
          goOn = false
        }
      }
    }
    if (firstGood == -1 || lastGood == -1)
      ""
    else
      in.substring(firstGood, lastGood + 1)
  }

  def leftTrim(in: String): String = {
    var goOn = true
    var firstGood = -1
    (0 until in.length) foreach { i =>
      val c: Char = in(i)
      if (goOn) {
        if (Character.isWhitespace(c) == false) {
          firstGood = i
          goOn = false
        }
      }
    }
    if (firstGood == -1) ""
    else in.substring(firstGood, in.length)
  }

  def rightTrim(in: String): String = {
    var goOn = true
    var lastGood = -1
    (in.length - 1 to 0 by -1) foreach { i =>
      val c: Char = in(i)
      if (goOn) {
        if (Character.isWhitespace(c) == false) {
          lastGood = i
          goOn = false
        }
      }
    }
    if (lastGood == -1) ""
    else in.substring(0, lastGood + 1)
  }

  def preprocess(in: String): String = {
    in.replaceAll(Pattern.quote("""\`"""), NullStr)
  }

  def postprocess(in: String): String = {
    in.replaceAll(NullStr, "`")
  }
}
