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

class varianceSpec extends MatchersSpecification {
  "the variance of matchers" should {
    "enable matcher types that should in theory be compatible to be anded/ored/xored together" in {
      val map = Map("one" -> 1, "two" -> 2, "three" -> 3)
      map must (contain ("two" -> 2) and haveKey ("two"))
      map must (contain ("two" -> 2) or haveKey ("two"))
      map must (contain ("two" -> 2) xor haveKey ("seven"))
      map must (haveKey ("two") and contain ("two" -> 2))
      map must (haveKey ("two") or contain ("two" -> 2))
      map must (haveKey ("seven") xor contain ("two" -> 2))
    }
    "allow matchers to be and-ed even with structural types" in {
      "hello" must haveSize(5)
      // "hello" is a { def size: Int }
      // however it is not possible to write
      // "hello" must (haveSize(5) and include("h")) because include needs a string 
      "hello" must (include("h") and haveSize(5)) 
      
      // in that case we case use haveLength which works on java.lang.String
      "hello" must (haveLength(5) and include("h"))
      "hello" must (include("h") and haveLength(5))
    }
    "allow matchers to be and-ed even with structural types and separated words" in {
      "hello" must have size(5)
      // "hello" is a { def size: Int }
      // however it is not possible to write
      "hello" must have size(5) and include("h") 
      "hello" must include("h") and have size(5) 
      
      // in that case we case use haveLength which works on java.lang.String
      "hello" must have length(5) and include("h")
      "hello" must include("h") and have length(5)
    }
  }
}
