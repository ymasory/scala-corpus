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
import org.specs.matcher.MatcherUtils._

/**
 * This trait provides functions which can be use on matchers: not, verifyAll, verifyAny
 */
trait LogicalMatchers {

  /**
   * @param m a matcher 
   * @return m.not
   */   
  def not[T](m: Matcher[T]) = m.not

  /**
   * @return a Matcher which combines all matchers with a logical 'and'
   * @return a 'true' matcher (always true) if the list is empty
   */   
  def verifyAll[T](ms: Iterable[Matcher[T]]): Matcher[T] = {
    ms match {
      case Nil => new Matcher[T](){ def apply(a: => T) = (true, "no matchers", "no matchers") }
      case m::Nil => m
      case m::rest => m.and(verifyAll(rest))
    }
  }
  /**
   * Alias of verifyAll with variable arguments
   */   
  def verifyAll[T](ms: Matcher[T]*): Matcher[T] = verifyAll(ms.toList)

  /**
   * @return a Matcher which combines all matchers with a logical 'or'
   * @return a 'false' matcher (always false) if the list is empty
   */   
  def verifyAny[T](ms: Iterable[Matcher[T]]): Matcher[T] = {
    ms match {
      case Nil => new Matcher[T](){ def apply(a: => T) = (false, "no matchers", "no matchers") }
      case m::Nil => m
      case m1::m2::Nil => m1.or(m2)
      case m::rest => m.or(verifyAny(rest))
    }
  }
  /**
   * Alias of verifyAny with variable arguments
   */   
  def verifyAny[T](ms: Matcher[T]*): Matcher[T] = verifyAny(ms.toList)
}
