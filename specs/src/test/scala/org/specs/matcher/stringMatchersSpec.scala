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

class stringMatchersSpec extends MatchersSpecification {
  "String matchers" should { 
    "provide a 'must_==/' matcher: 'hello' must_==/ 'HeLLo' " +
    "[alias: must beEqualToIgnoringCase]" in {
      "string" must_==/ "sTring"
      "string" must beEqualToIgnoringCase("sTring")
      expectation("string" must_==/ "striNg2") must failWith("'string' is not equal ignoring case to 'striNg2'")
      expectation("string" aka "the string" must be_==/("striNg2")) must failWith("the string 'string' is not equal ignoring case to 'striNg2'")
    }
    "provide a 'must_!=/' matcher: 'name' must_!=/ 'naME' will fail" +
    "[alias: notEqualIgnoreCase]" in {
      "string" must_!=/ "sTring2"
      "string" must notEqualIgnoreCase("sTring2")
      expectation("string" must_!=/ "strinG") must failWith("'string' is equal ignoring case to 'strinG'")
      expectation("string" aka "the string" must be_!=/("strinG")) must failWith("the string 'string' is equal ignoring case to 'strinG'")
    }
    "provide a 'beEqualToIgnoringSpace' matcher: 'hello' must beEqualToIgnoringSpace ' hello\t' " in {
      "string" must beEqualToIgnoringSpace(" string ")
      expectation("string" must beEqualToIgnoringSpace(" string2")) must failWith("'string' is not equal ignoring space to ' string2'")
      expectation("string" aka "the string" must beEqualToIgnoringSpace(" string2")) must failWith("the string 'string' is not equal ignoring space to ' string2'")
    }
    "provide a 'beMatching' matcher to match a pattern inside a string: " +
    " 'name' must beMatching('.*am.*') [alias: mustMatch]" in {
      "name" must beMatching(".*am.*")
      "name" mustMatch "name"
      expectation("name" must beMatching("xxx")) must failWith("'name' doesn't match 'xxx'")
      expectation("name" aka "the string" must beMatching("xxx")) must failWith("the string 'name' doesn't match 'xxx'")
    }
    "provide a 'notBeMatching' matcher not to match a pattern inside a string [alias: mustNotMatch]" in {
      "name" must notBeMatching("abc")
      "name" mustNotMatch "abc"
      expectation("name" must notBeMatching("n")) must failWith("'name' matches 'n'")
      expectation("name" aka "the string" must notBeMatching("n")) must failWith("the string 'name' matches 'n'")
    }
    "provide a 'include' matcher: 'name' must include('am')" in {
      "name" must include("am")
      expectation("name" must include("oo")) must failWith("'name' doesn't include 'oo'")
      expectation("name" aka "the string" must include("oo")) must failWith("the string 'name' doesn't include 'oo'")
    }
    "provide a 'notInclude' matcher: 'name' must notInclude('oo')" in {
      "name" must notInclude("oo")
      expectation("name" must notInclude("am")) must failWith("'name' includes 'am'")
      expectation("name" aka "the string" must notInclude("am")) must failWith("the string 'name' includes 'am'")
    }
    "provide a 'startWith' matcher: 'name' must startWith('na')" in {
      "name" must startWith("na")
      expectation("name" must startWith("oo")) must failWith("'name' doesn't start with 'oo'")
      expectation("name" aka "the string" must startWith("oo")) must failWith("the string 'name' doesn't start with 'oo'")
    }
    "provide a 'notStartWith' matcher: 'name' must notStartWith('am')" in {
      "name" must notStartWith("oo")
      expectation("name" must notStartWith("na")) must failWith("'name' starts with 'na'")
      expectation("name" aka "the string" must notStartWith("na")) must failWith("the string 'name' starts with 'na'")
    }
    "provide a 'endWith' matcher: 'name' must endWith('me')" in {
      "name" must endWith("me")
      expectation("name" must endWith("oo")) must failWith("'name' doesn't end with 'oo'")
      expectation("name" aka "the string" must endWith("oo")) must failWith("the string 'name' doesn't end with 'oo'")
    }
    "provide a 'notEndWith' matcher: 'name' must notEndWith('oo')" in {
      "name" must notEndWith("oo")
      expectation("name" must notEndWith("me")) must failWith("'name' ends with 'me'")
      expectation("name" aka "the string" must notEndWith("me")) must failWith("the string 'name' ends with 'me'")
    }
    "provide a 'find' matcher: 'name' must find('n(.*)e')" in {
      "name" must find("n(.*)e")
      expectation("name" must find("z")) must failWith("'z' isn't found in 'name'")
      expectation("name" aka "the string" must find("z")) must failWith("'z' isn't found in the string 'name'")
    }
    "provide a 'find' matcher: 'lallbl' must find('l(.*?)l').withGroups('a', 'b')" in {
      "lallbl" must find("l(.*?)l").withGroups("a", "b")
      expectation("zazbz" must find("l(.*?)l").withGroups("a", "b")) must failWith("'l(.*?)l' isn't found in 'zazbz' with groups 'a, b'. Found nothing")
      expectation("zazbz" aka "the string" must find("l(.*?)l").withGroups("a", "b")) must failWith("'l(.*?)l' isn't found in the string 'zazbz' with groups 'a, b'. Found nothing")
    }
    "provide a 'find' matcher: 'lallbl' must find('l(.*?)l').withGroup('a')" in {
      "lal" must find("l(.*?)l").withGroup("a")
      expectation("zazbz" must find("l(.*?)l").withGroup("a")) must failWith("'l(.*?)l' isn't found in 'zazbz' with group 'a'. Found nothing")
      expectation("zazbz" aka "the string" must find("l(.*?)l").withGroup("a")) must failWith("'l(.*?)l' isn't found in the string 'zazbz' with group 'a'. Found nothing")
    }
  }
  "String matchers with be or have as separated words" should beOkWith {
    "'be equalIgnoringCase'" in {
      "hello" must be equalIgnoringCaseTo("Hello")
      "hello" must not be equalIgnoringCaseTo("Hello2")
    }
    "'be ==/'" in {
      "hello" must be ==/("Hello")
      "hello" must not be ==/("Hello2")
    }
    "'be !=/'" in {
      "hello" must be !=/("Hello2")
      "hello" must not be !=/("Hello")
    }
    "'be equalIgnoringSpace'" in {
      "hello" must be equalIgnoringSpaceTo(" hello ")
      "hello" must not be equalIgnoringSpaceTo(" ello ")
    }
    "'not include'" in {
      "hello" must not include("zz")
    }
    "'be matching'" in {
      "hello" must be matching("h.*")
    }
    "'not be matching'" in {
      "hello" must not be matching("z.*")
    }
    "'not startWith'" in {
      "hello" must not startWith("z")
    }
    "'not endWith'" in {
      "hello" must not endWith("z")
    }
    "'have length'" in {
      "hello" must have length(5)
    }
    "'not have length'" in {
      "hello" must not have length(6)
    }
  }
  def beOkWith= addToSusVerb("be ok with ")
}
