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

import com.google.xml.combinators.Picklers._
import com.google.util.Utility.padInt

import java.text.ParseException
import java.util.{GregorianCalendar, TimeZone, Calendar}
import java.text.ParseException

import scala.util.parsing.combinator.{Parsers, ImplicitConversions}
import scala.util.parsing.input.CharArrayReader


/**
 * A class for representing normal play times, as defined by RFC 2326. It does not support
 * ranges, and fractional time is restricted to milliseconds (Media RSS needs).
 *
 * @see http://www.ietf.org/rfc/rfc2326.txt
 * @see http://search.yahoo.com/mrss
 * @author Iulian Dragos
 */
abstract class NormalPlayTime {
  /** The value in milliseconds. */
  def value: Long
  
  /** Is this object the special time 'now'? */
  def isNow: Boolean
}

/** A fixed time offset. */
case class SpecificTime(hour: Int, minute: Int, seconds: Int, millis: Int) extends NormalPlayTime {
  if (!(hour >= 0))
    throw new IllegalArgumentException("Invalid hour: " + hour)
  if (!(minute >= 0 && minute < 60))
    throw new IllegalArgumentException("Invalid minute: " + minute)
  if (!(seconds >= 0 && seconds < 60))
    throw new IllegalArgumentException("Invalid second: " + seconds)
  
  def isNow = false
  
  /** The value of this offset in milliseconds. */
  def value = (millis + seconds * DateTime.MILLIS_IN_SECOND
      + minute * DateTime.MILLIS_IN_MINUTE + hour * DateTime.MILLIS_IN_HOUR) 
    
  override def toString = {
    val sb = new StringBuilder
    if (hour < 100)
      padInt(sb, hour, 2)
    else
      sb.append(hour)
    sb.append(':')
    padInt(sb, minute, 2).append(':')
    padInt(sb, seconds, 2)
    if (millis > 0) sb.append('.').append(millis)
    sb.toString
  }
}

/** The specially designated time instant 'now'. @see http://www.ietf.org/rfc/rfc2326.txt */
case object Now extends NormalPlayTime {
  def value: Long = error("No 'value' in milliseconds for Now")
  
  def isNow = true
  
  override def toString = "now"
}

object NormalPlayTime {
  
  /**
   * Return a new NormalPlayTime instance based on the given String. It
   * accepts strings in the RFC 2326 format (roughly 'hh:mm:ss.millis' or
   * the special string 'now'.
   * 
   * @throws ParseException if the string does not follow the grammar.
   */
  def fromNptString(nptString: String): NormalPlayTime = {
    import DateParser._

    var str = nptString.trim
    if (str == "now") 
      Now
    else {
      val p = ((DateParser.intLiteral <~ ':')
            ~ (DateParser.oneOrTwoDigits <~ ':')
            ~ DateParser.oneOrTwoDigits
            ~ opt(secFraction) ^^ {
        case hour ~ min ~ second ~ Some(fraction) => 
	      new SpecificTime(hour, min, second, (fraction * 1000).toInt)
	    case hour ~ min ~ second ~ None => 
	      new SpecificTime(hour, min, second, 0)
      })
      p(new CharArrayReader(str.toArray)) match {
        case Success(v, _) => v
        case f: NoSuccess => throw new ParseException(f.toString, 0)
      }
    }
  }
  
  /** Return a new SpecificTime instance, given a positive offset in milliseconds. */
  def fromMillis(value: Long) = {
    if (value < 0) throw new IllegalArgumentException("Expected positive offset in milliseconds.")

    import DateTime.{MILLIS_IN_HOUR, MILLIS_IN_MINUTE, MILLIS_IN_SECOND}
    
    val (hour, rem1)   = (value / MILLIS_IN_HOUR,  value % MILLIS_IN_HOUR)
    val (min, rem2)    = (rem1  / MILLIS_IN_MINUTE, rem1 % MILLIS_IN_MINUTE)
    val (sec, millis)  = (rem2  / MILLIS_IN_SECOND, rem2 % MILLIS_IN_SECOND)
         
    new SpecificTime(hour.toInt, min.toInt, sec.toInt, millis.toInt)
  }
  
  /**
   * A pickler for NormalPlayTime in Media RSS format.
   * 
   * @throws PraseException when the string does not conform to the expected grammar.
   * @see NormalPlayTime.fromNptString
   */
  val pickler: Pickler[NormalPlayTime] = {
    def parsePlayTime(lit: String, in: St) = try {
      Success(NormalPlayTime.fromNptString(lit), in)
    } catch {
      case e: ParseException => Failure(e.getMessage, in)
    }
    
    filter(text, parsePlayTime, _.toString)
  }
}
