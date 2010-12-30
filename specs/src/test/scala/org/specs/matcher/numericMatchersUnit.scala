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
import org.scalacheck.Gen._

class numericMatchersUnit extends MatchersSpecification with ScalaCheck {
  "A 'beGreaterThan' matcher" should {
    "work with Longs" in {
      1000L must be_>=(500L)
    }
    "work with Floats" in {
      1000f must be_>=(500f)
    }
    "work with Doubles" in {
      1000.0 must be_>=(500.0)
    }
  }
  "A 'beClose' matcher" should {
    "be ok if x is inside the range n - delta, n + delta" in {
      case class RangeMatch(x: Double, n: Double, delta: Double)
      val cases  = for (n <- choose(-5, 5);
                        delta <- choose(0, 3);
                        x <- choose(n - delta, n + delta))
                        yield RangeMatch(x, n, delta)
      cases must pass { r: RangeMatch => val RangeMatch(x, n, delta) = r
        x must beCloseTo(n, delta)
      }
    }
  }
  "Numeric matchers" should {
    "not evaluate the expressions twice: be_>" in {
      be_>(1) must evalOnce(exp(1))
    }
    "not evaluate the expressions twice: be_>=" in {
      be_>=(1) must evalOnce(exp(1))
    }
    "not evaluate the expressions twice: be_<" in {
      be_<(1) must evalOnce(exp(1))
    }
    "not evaluate the expressions twice: be_<=" in {
      be_<=(1) must evalOnce(exp(1))
    }
    "not evaluate the expressions twice: be_closeTo" in {
      beCloseTo(1, 0) must evalOnce(exp(1))
    }
  }
}
