package org.specs.matcher
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
import org.specs.matcher.MatcherUtils._
import org.specs.specification.Result

/**
 * 
 */
trait EitherMatchers extends EitherBaseMatchers with EitherBeHaveMatchers
/**
 * 
 */
trait EitherBaseMatchers {
  def beRight[T](t: =>T) = new Matcher[Either[_, T]] {
    def apply(v: =>Either[_, T]) = {
      val value = v
      val expected = t
      (value == Right(t), 
       d(value) + " is Right with value" + q(expected),
       d(value) + " is not Right with value" + q(expected))
    }
  }
  def beLeft[T](t: =>T) = new Matcher[Either[T, _]] {
    def apply(v: =>Either[T, _]) = {
      val value = v
      val expected = t
      (value == Left(t), 
       d(value) + " is Left with value" + q(expected),
       d(value) + " is not Left with value" + q(expected))
    }
  }
}
trait EitherBeHaveMatchers { outer: EitherBaseMatchers => 
  implicit def toEitherResult[S, T](result: Result[Either[S,T]]) = new EitherResultMatcher(result)
  class EitherResultMatcher[S, T](result: Result[Either[S,T]]) {
    def right(r: =>T) = result.matchWithMatcher(outer.beRight(r))
    def left(l: =>S) = result.matchWithMatcher(outer.beLeft(l))
  }

}
