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
import org.specs.specification._
import org.specs.runner._
import org.specs._

class matchersUnit extends SpecificationWithJUnit with MatcherCases with ScalaCheck {
  "A matcher" should {
    "when negated, use the ok message of the original matcher to indicate a failure" in {
      val m = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok", "ko") }
      (m.not)(false)._3 must_== "ok"
    }
    "when combined with and, display a failure message like: 'ko message of m1' if the first matcher fails" in {
      val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (!b, "ok1", "ko1") }
      val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok2", "ko2") }
      (m1 and m2)(true)._3 must_== "ko1"
    }
    "when combined with and, display a failure message like: 'ok message of m1' but 'ko message of m2' if the second matcher fails" in {
      val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok1", "ko1") }
      val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (!b, "ok2", "ko2") }
      (m1 and m2)(true)._3 must_== "ok1 but ko2"
    }
    "when combined with and, display a success message like: 'ok message of m1' and 'ok message of m2' if no matchers fail" in {
      val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok1", "ko1") }
      val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok2", "ko2") }
      (m1 and m2)(true)._2 must_== "ok1 and ok2"
    }
  }
  "A matcher" can {
    "be combined with another matcher with a logical 'and' to provide a new matcher" in {
      matcherCases must pass { t: TestCase => val (a, m1, m2) = t
    	result((m1 and m2)(a)) mustBe result(m1(a)) && result(m2(a))
      }(set(minTestsOk->20))
    }
    "be combined with another matcher with a logical 'or' to provide a new matcher" in {
      matcherCases must pass { t: TestCase => val (a, m1, m2) = t
    	result((m1 or m2)(a)) mustBe result(m1(a)) || result(m2(a))
      }(set(minTestsOk->20))
    }
    "be combined with another matcher with a logical 'xor' to provide a new matcher" in {
      matcherCases must pass { t: TestCase => val (a, m1, m2) = t
        result((m1 xor m2)(a)) mustBe result(m1(a)) && !result(m2(a)) || !result(m1(a)) && result(m2(a))
      }(set(minTestsOk->20))
    }
    "be combined with another matcher with a logical 'not' to provide a new matcher" in {
      matcherCases must pass { t: TestCase => val (a, m1, m2) = t
        result(not(m1)(a)) mustBe !result(m1(a))
      }(set(minTestsOk->20))
    }
  }
}
import org.specs.Specification
import org.scalacheck.Gen._
import org.specs.Sugar._
trait MatcherCases {
  type TestCase = (Boolean, Matcher[Boolean], Matcher[Boolean])
  val matcherCases = for (b1 <- oneOf(true, false);
                          b2 <- oneOf(true, false);
                          a  <- oneOf(true, false);
                          val m1 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok1", "ko1") };
                          val m2 = new Matcher[Boolean](){ def apply(b: => Boolean) = (b, "ok2", "ko2")}
                        ) yield (a, m1, m2)
  case class MatcherCase(a: Boolean, m1: Matcher[Boolean], m2: Matcher[Boolean])
  def result(resultAndMessages: (Boolean, String, String)) = resultAndMessages._1
}
