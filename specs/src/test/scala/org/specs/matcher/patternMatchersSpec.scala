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

class patternMatchersSpec extends MatchersSpecification {
  "Pattern matchers" should { 
    "provide a beLike matcher using pattern matching: (1, 2) must beLike {case (1, _) => ok} " +
    "[ok is syntactic sugar for true from the Sugar trait]" in {
      "a" must beLike { case "a" => ok }
      ("a", "b") must beLike { case ("a", _) => ok }
      ("a", "b", "c") must beLike { case ("a", _, _) => ok }
      expectation(("a", "b", "c") must beLike { case ("a", "c", _) => ok }) must
                       failWith ("'(a,b,c)' doesn't match the expected pattern")

      expectation(("a", "b", "c") aka "the triplet" must beLike { case ("a", "c", _) => ok }) must
                       failWith ("the triplet '(a,b,c)' doesn't match the expected pattern")

      expectation(("a", "b", "c") must not(beLike { case ("a", _, _) => ok })) must
           failWith ("'(a,b,c)' matches the given pattern")
    }
    "provide a beNone matcher for options: List().find {_ == 2} must beNone" in {
      List().find {_ == 2} must beNone
      expectation(List(2).find {_ == 2} must beNone) must failWith("'Some(2)' is not None")
      expectation(List(2).find {_ == 2} aka "searching 2 in a list:" must beNone) must
                                          failWith("searching 2 in a list: 'Some(2)' is not None")
    }
    "provide a beAsNoneAs matcher matching if 2 options are None at the same time" in {
      val noneString: Option[String] = None
      noneString must beAsNoneAs(noneString)
      Some(2) must beAsNoneAs(Some(1))
      expectation(noneString must beAsNoneAs(Some("thing"))) must failWith("'Some(thing)' is not None")
      expectation(noneString aka "string presence" must beAsNoneAs(Some("thing"))) must failWith("string presence 'Some(thing)' is not None")

      expectation(Some("thing") must beAsNoneAs(noneString)) must failWith("'Some(thing)' is not None")
    }
    "provide a beSome matcher for options: List(2).find {_ == 2} must beSome[Int] [alias: beSomething when type is not important]" in {
      List(2).find {_ == 2} must beSome[Int]
      List(2).find {_ == 2} must beSomething
      expectation(List().find {_ == 2} must beSomething) must failWith("'None' is not Some(x)")
    }
    "provide a beSome(value) matcher checking the value of an option" in {
      List(2).find {_ == 2} must beSome(2)
      expectation(List().find {_ == 2} must beSome(2)) must failWith("'None' is not 'Some(2)'")
      expectation(List(2).find {_ == 2} must beSome(3)) must failWith("'Some(2)' is not 'Some(3)'")
    }
    "provide a be like matcher" in {
      "a" must be like { case "a" => ok }
    }
    "provide a be asNoneAs matcher" in {
      val noneString: Option[String] = None
      noneString must be asNoneAs(noneString)
      Some(2) must be asNoneAs(Some(1))
    }
  }
  "Pattern matchers" can {
    "specify a which clause to check additional properties: List('name').find {_ == 'name'} must beSome[String].which {_.size == 4}" in {
      List("name").find {_ == "name"} must beSome[String].which {_.size == 4}
    }
  }
}
