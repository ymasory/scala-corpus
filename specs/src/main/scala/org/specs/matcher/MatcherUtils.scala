/**
 * Copyright (c) 2007-2010 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.matcher
import java.util.regex._

/**
 * This object provides utility functions for matchers
 */
object MatcherUtils {

  /**
   * @return true if b is matching the regexp a
   */
  def matches[T <: String](a: String)(b: T) = {
    try { a != null && b != null && Pattern.compile(a).matcher(b).find }
    catch { case e: java.util.regex.PatternSyntaxException => false }
  } 

  /**
   * @return true if a string s can be parsed to an integer
   */
  def isInteger(s: String): Boolean = { try {s.toInt} catch { case _ => return false }; true }

  /**
   * @return an object.toString() between quotes (used in messages creation)
   */
  def q(a: Any)  = if (null == a) "'null'" else ("'" + a.toString + "'")
  
  /**
   * @return an object.toString() without quotes (used in messages creation)
   */
  def unq(a: Any)  = if (null == a) "null" else a.toString
}
