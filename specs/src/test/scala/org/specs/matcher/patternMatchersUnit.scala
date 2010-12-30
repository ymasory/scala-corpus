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
import org.specs.Sugar._

class patternMatchersUnit extends MatchersSpecification {
  "A 'beLike' pattern matcher" should {
    "be ok even with a null pattern" in {
      val pattern: PartialFunction[String, Boolean] = { case a => false }
      expectation("name" must beLike(pattern)) must failWith("'name' doesn't match the expected pattern")
    }
    "be ok even with a null value" in {
      val name : String = null
      expectation(name must beLike { case s: String => ok }) must failWith("'null' doesn't match the expected pattern")
    }
    "not evaluate the expressions twice" in {
      val anyValue: Any = 1
      beLike { (s:Any) => s match { case _ => ok } } must evalOnce(exp(anyValue))
    }
  }
  "A 'beSome' option matcher" should {
    "be ok even with a null value" in {
      val value : Option[String] = null
      expectation(value must beSome[String]) must failWith("'null' is not Some(x)")
    }
    "be ok even with a null pattern for the which function" in {
      val pattern : (Any => Boolean) = null
      expectation(Some("name") must beSome[String].which(pattern)) must failWith("the 'which' property is a null function")
    }
    "not evaluate the expressions twice" in {
      val some: Option[Int] = Some(1)
      beSome[Int] must evalOnce(exp(some))
    }
  }
  "A 'beNone' option matcher" should {
    "not evaluate the expressions twice" in {
      val nothing: Option[Any] = None
      beNone must evalOnce(exp(nothing))
    }
  }
}
