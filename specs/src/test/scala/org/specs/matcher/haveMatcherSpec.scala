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
import org.specs._
import org.specs.specification._
import org.specs.runner._

class haveMatcherSpec extends SpecificationWithJUnit { outer =>
  "A collection matcher starting with 'have' can be used with have as a separated word" in {
    List("hello") must have size(1)
  }
  "A collection matcher starting with 'notHave' can be used with 'not have' as a separated words" in {
    List("hello") must not have size(2)
  }
  "A collection matcher starting with 'notHave' can be used with 'not have' as a separated words" in {
    List("hello") must not have(size(2))
  }
  "A collection matcher starting with 'have' can be used with have as a separated word" in {
    List(1) must have size(1)
  }
  "A collection matcher starting with 'notHave' can be used with 'not have' as a separated words" in {
    List(1) must not have(size(2))
  }

  def possibly[T] = new PossiblyMatcher[T]
  class PossiblyMatcher[T] extends Matcher[T] { 
    def apply(v: =>T) = (true, "", "")
    def ? (m: Matcher[T]) = m
  }
  implicit def toPossiblyMatcherDecorator[T](m: Matcher[T]): PossiblyMatcherDecorator[T] = new PossiblyMatcherDecorator(m)
  class PossiblyMatcherDecorator[T](m: Matcher[T]) extends Matcher[T] { 
    def apply(v: =>T) = m(possibleMatch(v))
    def possibleMatch(v: =>T) = v
    def possibly = this
  }
  implicit def toPossiblyMatchable[T](m: Matcher[T]): PossiblyMatchable[T] = new PossiblyMatchable(m)
  class PossiblyMatchable[T](m: Matcher[T]) { 
    def :: (e: PossiblyMatcher[Nothing]) = m // add the possibly logic here
  }
  implicit def toPossiblyMatcherResult[T](result: Result[T]): PossiblyMatcherResult[T] = new PossiblyMatcherResult(result)
  class PossiblyMatcherResult[T](result: Result[T]) {
    def be_==(a: T) = result.matchWith(possibly(outer.be_==(a)))
    def be(a: T) = result.matchWith(possibly(outer.be(a)))
    private def possibly(m: Matcher[T]) = m  // add some logic for the semantics of "possibly"
  }    

  "An extension for a matcher can be created" in {
    "A string" must possibly be_== "A string"
  }
  "An extension for a matcher can be created" in {
    "A string" must be_==("A string").possibly
  }
  "An extension for a matcher can be created" in {
    "A string" must possibly :: be_==("A string")
  }
  "An extension for a matcher can be created" in {
    "A string" must possibly ? be_==("A string")
  }
}
