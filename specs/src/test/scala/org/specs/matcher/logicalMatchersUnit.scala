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

class logicalMatchersUnit extends MatchersSpecification {
  "A 'verifyAll' matcher" should {
    "return a True matcher with an empty list" in {
      verifyAll(List())(true) must_== (true, "no matchers", "no matchers")
      verifyAll(List())(false) must_== (true, "no matchers", "no matchers")
    }
    "return the first matcher of the list if the list contains only one element" in {
      val m = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok", "ko") }
      verifyAll(m) mustBe m
    }
    "return a matcher m1.and(m2) for a list of two elements List(m1, m2)" in {
      val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok1", "ko1") }
      val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok2", "ko2") }
      verifyAll(m1, m2)(true) must_== m1.and(m2)(true)
      verifyAll(m1, m2)(false) must_== m1.and(m2)(false)
    }
  }
  "A 'verifyAny' matcher" should {
    "return a False matcher with an empty list" in {
      verifyAny(List())(true) must_== (false, "no matchers", "no matchers")
      verifyAny(List())(false) must_== (false, "no matchers", "no matchers")
    }
    "return the first matcher of the list if the list contains only one element" in {
      val m = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok", "ko") }
      verifyAny(m) mustBe m
    }
    "return a matcher m1.or(m2) for a list of two elements List(m1, m2)" in {
      val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok1", "ko1") }
      val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok2", "ko2") }
      verifyAny(m1, m2)(true) must_== m1.or(m2)(true)
      verifyAny(m1, m2)(false) must_== m1.or(m2)(false)
    }
  }
}
