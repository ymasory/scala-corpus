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
import org.specs.runner._
  
class numericMatchersSpec extends MatchersSpecification {
  "Numeric matchers" should {
    "provide a 'be_<' matcher: 1 must be_<(2)" in {
      1 must be_<(2)
      expectation(1 must be_<(1)) must failWith("1 is not less than 1")
      expectation(1 aka "the number" must be_<(1)) must failWith("the number 1 is not less than 1")

      expectation(1.0 must be_<(1.0)) must failWith("1.0 is not less than 1.0")
    }
    "provide a 'beLessThan' matcher: 1 must beLessThan(2)" in {
      1 must beLessThan(2)
      expectation(1 must beLessThan(1)) must failWith("1 is not less than 1")
      expectation(1 aka "the number" must beLessThan(1)) must failWith("the number 1 is not less than 1")
    }
    "provide a 'be_<=' matcher: 1 must be_<=(2)" in {
      2 must be_<=(2)
      expectation(1 must be_<=(0)) must failWith("1 is greater than 0")
      expectation(1 aka "the number" must be_<=(0)) must failWith("the number 1 is greater than 0")
    }
    "provide a 'beLessThanOrEqualTo' matcher: 1 must beLessThanOrEqual(2)" in {
      2 must beLessThanOrEqualTo(2)
      expectation(1 must beLessThanOrEqualTo(0)) must failWith("1 is greater than 0")
      expectation(1 aka "the number" must beLessThanOrEqualTo(0)) must failWith("the number 1 is greater than 0")
    }
    "provide a 'be_>' matcher: 2 must be_>(1)" in {
      2 must be_>(1)
      expectation(1 must be_>(2)) must failWith("1 is less than 2")
      expectation(1 aka "the number" must be_>(2)) must failWith("the number 1 is less than 2")

      expectation(1.0 must be_>(2.0)) must failWith("1.0 is less than 2.0")
    }
    "provide a 'beGreaterThan' matcher: 2 must beGreaterThan(1)" in {
      2 must beGreaterThan(1)
      expectation(1 must beGreaterThan(2)) must failWith("1 is less than 2")
      expectation(1 aka "the number" must beGreaterThan(2)) must failWith("the number 1 is less than 2")
    }
    "provide a 'be_>=' matcher: 2 must be_>=(1)" in {
      2 must be_>=(1)
      expectation(0 must be_>=(1)) must failWith("0 is less than 1")
      expectation(0 aka "the number" must be_>=(1)) must failWith("the number 0 is less than 1")
    }
    "provide a 'beGreaterThanOrEqualTo' matcher: 2 must beGreaterThanOrEqualTo(1)" in {
      2 must beGreaterThanOrEqualTo(1)
      expectation(0 must beGreaterThanOrEqualTo(1)) must failWith("0 is less than 1")
      expectation(0 aka "the number" must beGreaterThanOrEqualTo(1)) must failWith("the number 0 is less than 1")
    }
    "provide a 'must beCloseTo' matcher: 1.2 must beCloseTo(1.0, 0.5)" in {
      1.2 must beCloseTo(1.0, 0.5)
      1 must beCloseTo(1, 1)
      expectation(1.2 must beCloseTo(1.0, 0.1)) must failWith("1.2 is not close to 1.0 +/- 0.1")
      expectation(1.2 aka "the number" must beCloseTo(1.0, 0.1)) must failWith("the number 1.2 is not close to 1.0 +/- 0.1")

      expectation(1.2 must not(beCloseTo(1.0, 0.2))) must failWith("1.2 is close to 1.0 +/- 0.2")
    }
    "provide a 'must beCloseTo' matcher: 1.2 must beCloseTo(1.0 +/- 0.5)" in {
      1.2 must be closeTo (1.0 +/- 0.5)
    }
    "provide a 'be <' matcher" in {
      1 must be <(2)
    }
    "provide a 'be lessThan' matcher" in {
      1 must be lessThan(2)
    }
    "provide a 'be <=' matcher" in {
      2 must be <=(2)
    }
    "provide a 'be lessThanOrEqualTo' matcher" in {
      2 must be lessThanOrEqualTo(2)
    }
    "provide a 'be >(1)' matcher" in {
      2 must be >(1)
      expectation(2 must be >(2)) must failWith("2 is equal to 2")
    }
    "provide a 'be > 1' matcher" in {
      2 must be > 1
      expectation(2 must be > 2) must failWith("2 is equal to 2")
    }
    "provide a 'be greaterThan' matcher" in {
      2 must be greaterThan(1)
    }
    "provide a 'be >=' matcher" in {
      2 must be >=(1)
    }
    "provide a 'be greaterThanOrEqualTo' matcher" in {
      2 must be greaterThanOrEqualTo(1)
    }
    "provide a 'must be closeTo' matcher" in {
      1.2 must be closeTo(1.0, 0.5)
    }
    "provide a 'must be ~' matcher" in {
      1.2 must be ~(1.0, 0.5)
    }
  }
}
