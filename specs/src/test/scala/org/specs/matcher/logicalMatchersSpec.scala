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

class logicalMatchersSpec extends MatchersSpecification {

  "Logical matchers" should {
    "provide a not function to negate a matcher. For example 'name' must not(beMatching('abc'))" in {
      "name" must not(beMatching("abc"))
      expectation("name" must not(beMatching("n"))) must failWith("'name' matches 'n'")
      expectation("name" aka "the string" must not(beMatching("n"))) must failWith("the string 'name' matches 'n'")
    }
    "provide an 'and' method to combine matchers with a logical and. The 'and' evaluation is stopped if the first argument is false" +
    "'string' must (beMatching('s') and beMatching('g'))" in {
      "string" must (beMatching("s") and beMatching("g"))
      expectation("string" must (beMatching("z") and beMatching("f"))) must
                failWith("'string' doesn't match 'z'")
      expectation("string" aka "the string" must (beMatching("z") and beMatching("f"))) must
                failWith("the string 'string' doesn't match 'z'")

      expectation("string" must (beMatching("s") and beMatching("f"))) must
                failWith("'string' matches 's' but 'string' doesn't match 'f'")
      expectation("string" aka "the string" must (beMatching("s") and beMatching("f"))) must
                failWith("the string 'string' matches 's' but the string 'string' doesn't match 'f'")
    }
    "provide an 'or' method to combine matchers with a logical or. The 'or' evaluation is done for each operand " +
    "'string' must (beMatching('s') or beMatching('g'))"  in {
      "string" must (beMatching("s") or beMatching("g"))
      "string" must (beMatching("s") or beMatching("z"))
      "string" must (beMatching("z") or beMatching("s"))
      expectation("string" must (beMatching("z") or beMatching("f"))) must
                failWith("'string' doesn't match 'z' and 'string' doesn't match 'f'")
      expectation("string" aka "the string" must (beMatching("z") or beMatching("f"))) must
                failWith("the string 'string' doesn't match 'z' and the string 'string' doesn't match 'f'")
    }
    "provide a 'verifyAll' method which is ok if every matcher is ok"  in {
        "string" must verifyAll (beMatching("s"), beMatching("g"))
        expectation("string" must verifyAll (beMatching("s"), beMatching("f"))) must
                  failWith("'string' matches 's' but 'string' doesn't match 'f'")
        expectation("string" aka "the string" must verifyAll (beMatching("s"), beMatching("f"))) must
                  failWith("the string 'string' matches 's' but the string 'string' doesn't match 'f'")
    }
    "provide a 'verifyAny' method which is ok if at least one matcher is ok"  in {
      "string" must verifyAny (beMatching("s"), beMatching("z"))
      expectation("string" must verifyAny (beMatching("z"), beMatching("f"))) must
                failWith("'string' doesn't match 'z' and 'string' doesn't match 'f'")
      expectation("string" aka "the string" must verifyAny (beMatching("z"), beMatching("f"))) must
                failWith("the string 'string' doesn't match 'z' and the string 'string' doesn't match 'f'")
    }
  }
}
