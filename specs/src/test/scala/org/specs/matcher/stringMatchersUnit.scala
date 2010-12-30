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

class stringMatchersUnit extends MatchersSpecification {
  "An equalsIgnoreCase matcher" should {
    "be ok even with null values" in {
      val s: String = null
      expectation("name" must beEqualToIgnoringCase(s)) must failWith("'name' is not equal ignoring case to 'null'")
      expectation(s must beEqualToIgnoringCase("name")) must failWith("'null' is not equal ignoring case to 'name'")
    }
    "not evaluate the expressions twice" in {
      beEqualToIgnoringCase("") must evalOnce(exp(""))
    }
  }
  "An include matcher" should {
    "be ok even with null values" in {
      val s: String = null
      expectation("name" must include(null)) must failWith("'name' doesn't include 'null'")
      expectation(s must include("name")) must failWith("'null' doesn't include 'name'")
    }
    "not evaluate the expressions twice" in {
      include[String]("") must evalOnce(exp(""))
    }
  }
  "A beMatching matcher" should {
    "be ok even with null values" in {
      val s: String = null
      expectation("name" must beMatching(s)) must failWith("'name' doesn't match 'null'")
      expectation(s must beMatching("name")) must failWith("'null' doesn't match 'name'")
    }
    "not evaluate the expressions twice" in {
      beMatching("") must evalOnce(exp(""))
    }
  }
  "A startWith matcher" should {
    "be ok even with null values" in {
      val s: String = null
      expectation("name" must startWith(s)) must failWith("'name' doesn't start with 'null'")
      expectation(s must startWith("name")) must failWith("'null' doesn't start with 'name'")
    }
    "not evaluate the expressions twice" in {
      startWith("") must evalOnce(exp(""))
    }
  }
  "A endWith matcher" should {
    "be ok even with null values" in {
      val s: String = null
      expectation("name" must endWith(s)) must failWith("'name' doesn't end with 'null'")
      expectation(s must endWith("name")) must failWith("'null' doesn't end with 'name'")
    }
    "not evaluate the expressions twice" in {
      endWith("") must evalOnce(exp(""))
    }
  }
}
