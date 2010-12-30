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
package org.specs.samples
import org.specs._
import org.specs.specification._
import org.specs.Sugar._
import org.specs.runner._

object sampleSpecification2 extends Specification {
  "A sample specification2" should {
    "return something" in {
       "hello" mustBe "hello"
    }
  }
}


object allSpecification extends Specification {
  def length11(s: String) = new Specification(s) {
    s should {
      "have 11 characters" in {
        s.length must_== 11
      }
    }
  }
  include(length11("hello world"), length11("cold today!"))
}


class helloWorldSpec extends SpecificationWithJUnit("Hello world") with ScalaTest {
  "'hello world' has 11 characters" in {
     "hello world".size mustBe 11
  }
  "'hello world' matches 'h.* w.*'" in {
    "hello world" must beMatching("h.* w.*")
  } tag "excluded"
}

class helloWorldSpecification extends Specification("Hello world") with ScalaTest {
  "The hello world string" should {
    "have 11 characters" in {
       "hello world".size mustBe 12
    }
    "match 'h.* w.*'" in {
      "hello world" must beMatching("h.* w.*")
    }
    "not throw an error" in {
      error("this is an error")
    }
    "have a skipped example" in {
      skip("this is skipped")
    }
  }
}

import org.specs.mock._
import java.io._
object expectationsOnly extends Specification("Hello world") with JMocker with ClassMocker {
  "hello world".size mustBe 11
  3 must_== { "abc".size }
  classOf[OutputStream].expectsOne(_.flush) in { _.flush }
  "this example should also work" in { classOf[OutputStream].expectsOne(_.flush) in { _.flush} }
}
class expectationsOnlyTest extends JUnit4(expectationsOnly)

