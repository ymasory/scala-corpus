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
  
class beMatcherSpec extends MatchersSpecification { outer =>
  "A matcher can be 'and-ed' with another 'be' matcher" in {
    "hello" must be equalTo("hello") and be equalTo("hello")
    expectation("hello" must be equalTo("hello") and be equalTo("hello2")) must failWithMatch("hello2")
  }
  "A matcher can be 'and-ed' with another 'not be' matcher" in {
    "hello" must be equalTo("hello") and not be equalTo("hello2")
    "hello" must not be equalTo("world") and be equalTo("hello")
    expectation("hello" must be equalTo("hello") and not be equalTo("hello")) must failWithMatch("hello")
    expectation("hello" must be equalTo("world") and not be equalTo("hello")) must failWithMatch("world")
   }
  "A 'not be' matcher can be 'and-ed' with another 'not be' matcher" in {
    "hello" must not be equalTo("hello2") and not be equalTo("hello2")
    expectation("hello" must not be equalTo("hello2") and not be equalTo("hello")) must failWithMatch("hello")
  }
  "A matcher can be 'or-ed' with another 'be' matcher" in {
    "hello" must be equalTo("hello") or be equalTo("hello2")
    "world" must be equalTo("world2") or be equalTo("world")
    expectation("hello" must be equalTo("hello2") or be equalTo("hello2")) must failWithMatch("hello2")
  }
  "A matcher can be 'or-ed' with another 'not be' matcher" in {
    "hello" must be equalTo("hello") or not be equalTo("hello2")
  }
  "A matcher can be 'xor-ed' with another 'be' matcher" in {
    "hello" must be equalTo("universe") xor be equalTo("hello")
    "hello" must be equalTo("hello") xor be equalTo("world")
    expectation("hello" must be equalTo("hello") xor be equalTo("hello")) must failWithMatch("hello")
    expectation("hello" must be equalTo("world") xor be equalTo("universe")) must failWithMatch("hello")
  }
  "A matcher starting with 'be' can be used with 'be' as a separated word" in {
    "hello" must be equalTo("hello") 
    expectation("hello" must be equalTo("hello2")) must failWithMatch("hello2")
  }
  "A matcher starting with 'notBe' can be used with 'not be' as a separated word" in {
    "hello" must not be equalTo("world") 
    expectation("hello" must not be equalTo("hello")) must failWithMatch("hello")
  }
  "not be ==" in {
    "hello" must not be ==("world") 
    expectation("hello" must not be ==("hello")) must failWithMatch("hello")
  }
  "be asNullAs" in {
    var s: String = null
    var s2: String = null
    s must be asNullAs(s2)
    expectation(s must be asNullAs("")) must failWithMatch("null")
  }
  "not be asNullAs" in {
    var s: String = null
    var s2: String = ""
    s must not be asNullAs(s2)
    expectation(s must not be asNullAs(null)) must failWithMatch("null")
  }
  "be in" in {
    "hello" must be in(List("hello")) 
    expectation("hello" must be in(List("hello2"))) must failWithMatch("hello2")
  }
  "not be in" in {
    "hello" must not be in(List("world")) 
    expectation("hello" must not be in(List("hello"))) must failWithMatch("hello")
  }
  "be oneOf" in {
    "hello" must be oneOf("hello", "world") 
    expectation("hello" must be oneOf("hi", "world")) must failWithMatch("world")
  }
  "not be oneOf" in {
    "hello" must not be oneOf("world") 
    expectation("hello" must not be oneOf("hello", "world")) must failWithMatch("world")
  }
  "be this or that must provide a failure message showing all errors and successes" in {
    expectation("hello" must be oneOf("a", "b") or be oneOf("c")) must failWithMatch("not one of 'a, b'")
  }
}