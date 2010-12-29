/* Copyright (c) 2008 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


package com.google.gdata.data.util

import scala.util.parsing.combinator.{Parsers, ImplicitConversions}
import scala.util.parsing.input.CharArrayReader

import java.text.ParseException

/**
 * Parse a date that follows RFC 3339 (used by the Atom spec)
 */
object DateParser extends Parsers with ImplicitConversions {
  type Elem = Char
    
  lazy val digit: Parser[Int] = elem("digit", _.isDigit) ^^ (_ - '0')
  lazy val sign:  Parser[Char] = elem('+') | elem('-')
  
  /** Parse a positive integer literal. */
  lazy val intLiteral: Parser[Int] = rep1(digit) ^^ { case ds => ds.foldLeft(0)(_ * 10 + _) }
    
  lazy val fullYear: Parser[Int] = 
    digit ~ digit ~ digit ~ digit ^^ { case a ~ b ~ c ~ d => a * 1000 + b * 100 + c * 10 + d }
    
  lazy val twoDigits: Parser[Int] = digit ~ digit ^^ {case a ~ b => a * 10 + b }
  lazy val oneOrTwoDigits: Parser[Int] = digit ~ opt(digit) ^^ {
    case a ~ Some(b) => a * 10 + b
    case a ~ None => a
  }
  
  lazy val month = 
    twoDigits >> { n => if (n > 0 && n < 13) success(n) else failure("Invalid month: " + n) }
    
  lazy val day = 
    twoDigits >> { n => if (n > 0 && n < 32) success(n) else failure("Invalid day of the month: " + n) }
    
  lazy val hour = 
    twoDigits >> { n => if (n >= 0 && n < 24) success(n) else failure("Invalid hour: " + n) }
    
  lazy val minute =
    twoDigits >> { n => if (n >= 0 && n < 61) success(n) else failure("Invalid minute: " + n) }
      
  lazy val second =
    twoDigits >> { n => if (n >= 0 && n < 61) success(n) else failure("Invalid second: " + n) }
    
  lazy val secFraction =
    (elem('.') ~ rep1(digit)) ^^ { 
      case p ~ rest => (p :: rest).mkString("", "", "").toDouble
    }

  case class Offset(sign: Char, hour: Int, minute: Int) {
    def toMillis = {
      val res = (hour * 60 + minute) * 60000 
      if (sign == '-') -res else res
    }
  }
  object ZuluOffset extends Offset('+', 0, 0)

  lazy val numOffset = sign ~ hour ~ (elem(':') ~> minute) ^^ Offset    
  lazy val offset = elem('Z') ^^^ ZuluOffset | numOffset

  lazy val date = (fullYear <~ '-') ~ (month <~ '-') ~ day
  lazy val time = (hour <~ ':') ~ (minute <~ ':') ~ second  
  
  lazy val dateTime: Parser[DateTime] = (date <~ 'T') ~ time ~ opt(secFraction) ~ offset ^^ {
    case (y ~ m ~ d) ~ (h ~ min ~ sec) ~ frac ~ offset =>
      DateTime(y, m, d, h, min, sec, frac, offset.toMillis)
  }

  lazy val dateOnly: Parser[DateTime] = date ^^ {
    case y ~ m ~ d =>
      DateTime(y, m, d)
  }
  
  /** 
   * Parse a date/time in RFC 3339 format.
   *
   * @throws ParseException when the string is not a proper date/time. 
   */
  def parseDateTime(str: String): DateTime = {
    val input = new CharArrayReader(str.toArray)
    phrase(dateTime)(input) match {
      case Success(dt, _) => dt
      case f => throw new ParseException(f.toString, 0)
    }
  }
  
  /**
   * Parse a date only, in RFC 3339 format. It returns a DateTime instance with the
   * 'dateOnly' flag set.
   * 
   * @throws ParseException when the string is not a proper date/time.
   */
  def parseDate(str: String): DateTime = {
    val input = new CharArrayReader(str.toArray)
    phrase(dateOnly)(input) match {
      case Success(dt, _) => dt
      case f => throw new ParseException(f.toString, 0)
    }
  }
  
  /**
   * Parse a date or a date time, in RFC 3339 format. If the given string has a time
   * component, the returned DateTime will have the 'dateOnly' flag set to false and
   * the time part taken into account.
   * 
   * @throws ParseException when the string fails parsing.
   */
  def parseDateOrDateTime(str: String): DateTime = {
    val input = new CharArrayReader(str.toArray)
    (phrase(dateTime) | phrase(dateOnly))(input) match {
      case Success(dt, _) => dt
      case f => throw new ParseException(f.toString, 0)
    }
  }
}
