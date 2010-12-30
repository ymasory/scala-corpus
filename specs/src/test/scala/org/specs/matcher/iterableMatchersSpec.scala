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
import org.specs._
import org.specs.specification.fullDetails

class iterableMatchersSpec extends MatchersSpecification {
  "Iterable matchers" should {
    "provide a 'must beEmpty' matcher on iterables: List() must beEmpty" in {
      List() must beEmpty
      expectation(List("1") must beEmpty) must failWith("List(1) is not empty")
      expectation(List("1") aka "the list" must beEmpty) must failWith("the list List(1) is not empty")
    }
    "provide a 'must notBeEmpty' matcher on iterables: List(1) must notBeEmpty" in {
      List("1") must notBeEmpty
      expectation(List() must notBeEmpty) must failWith("List() is empty")
      expectation(List() aka "the list" must notBeEmpty) must failWith("the list List() is empty")
    }
    "provide a 'must contain' matcher on iterables: List(1) must contain(1) [alias: mustContain]" in {
      List("one", "two") must contain("one")
      expectation(List("one", "two") mustContain "three") must failWith("'List(one, two)' doesn't contain 'three'")
      expectation(List("one", "two") aka "the list" must contain("three")) must failWith("the list 'List(one, two)' doesn't contain 'three'")
    }
    "provide a 'must notContain' matcher on iterables: List(1) must notContain(2) [alias: mustNotContain]" in {
      List("one", "two") must notContain("three")
      expectation(List("one", "two") mustNotContain "one") must failWith("'List(one, two)' contains 'one'")
      expectation(List("one", "two") aka "the list" must notContain("one")) must failWith("the list 'List(one, two)' contains 'one'")
    }
    "provide a 'must containAll' matcher on iterables: List(1, 2, 3) must containAll(List(1, 2))" in {
      List("one", "two", "three") must containAll(List("one", "two"))
      expectation(List("one", "two") must containAll(List("one", "three"))) must failWith("'List(one, two)' doesn't contain all of 'List(one, three)'")
    }
    "provide a 'must containInOrder' matcher on iterables checking if one sequence is included inside another" in {
      List("one", "two", "three") must containInOrder(List("one", "two"))
      expectation(List("one", "two") must containInOrder(List("two", "one"))) must failWith("'List(one, two)' doesn't contain all of 'List(two, one)' in order")
    }
    "provide a 'must beIn' matcher on iterables: 'one' must beIn(List('one', 'two'))" in {
      "one" must beIn(List("one", "two"))
      expectation("three" must beIn(List("one", "two"))) must failWith("'three' is not in 'List(one, two)'")
      expectation("three" aka "the element" must beIn(List("one", "two"))) must failWith("the element 'three' is not in 'List(one, two)'")
    }
    "provide a 'must notBeIn' matcher on iterables: 'three' must notBeIn(List('one', 'two'))" in {
      "three" must notBeIn(List("one", "two"))
      expectation("one" must notBeIn(List("one", "two"))) must failWith("'one' is in 'List(one, two)'")
      expectation("one" aka "the element" must notBeIn(List("one", "two"))) must failWith("the element 'one' is in 'List(one, two)'")
    }
    "provide a 'must beOneOf' matcher on iterables: 'one' must beOneOf('one', 'two')" in {
      "one" must beOneOf("one", "two")
      expectation("three" must beOneOf("one", "two")) must failWith("'three' is not one of 'one, two'")
      expectation("three" aka "the element" must beOneOf("one", "two")) must failWith("the element 'three' is not one of 'one, two'")
    }
    "provide a 'must notBeOneOf' matcher on iterables: 'three' must notBeOneOf('one', 'two')" in {
      "three" must notBeOneOf("one", "two")
      expectation("one" must notBeOneOf("one", "two")) must failWith("'one' is one of 'one, two'")
      expectation("one" aka "the element" must notBeOneOf("one", "two")) must failWith("the element 'one' is one of 'one, two'")
    }
    "provide a 'must have' matcher on iterables: List('one', 'two') must have {m: String => m.contains('w')} [alias: mustExist]" in {
      List("one", "two") must have((_:String).contains("w"))
      expectation(List("one", "two") mustHave((_:String).contains("z"))) must failWith("no element verifies the property in 'List(one, two)'")
      expectation(List("one", "two") aka "the list" must have((_:String).contains("z"))) must failWith("no element verifies the property in the list 'List(one, two)'")
    }
    "provide a 'must notHave' matcher on iterables: List('one', 'two') must notExist {m: String => m.contains('z')}  [alias: mustNotExist]" in {
      List("one", "two") must notHave((_:String).contains("z"))
      expectation(List("one", "two") aka "the list" must notHave((_:String).contains("n"))) must failWith("at least one element verifies the property in the list 'List(one, two)'")
    }
    "provide a 'must containMatch' matcher on iterables: checking if it contains a string including a regexp: " +
    "List('one', 'two') must containMatch('[n-o]') [alias: mustContainMatch]" in {
      List("one", "two") must containMatch("[n-o]")
      expectation(List("one", "two") mustContainMatch("[a-c]")) must failWith("no element matches '[a-c]' in 'List(one, two)'")
      expectation(List("one", "two") aka "the list" must containMatch("[a-c]")) must failWith("no element matches '[a-c]' in the list 'List(one, two)'")
    }
    "provide a 'must notContainMatch' matcher checking if it doesn't contain a string including a regexp: " +
    "List('one', 'two') must containMatch('[a-c]') [alias: mustNotContainMatch]" in {
      List("one", "two") must notContainMatch("[a-c]")
      expectation(List("one", "two") mustNotContainMatch("[n-o]")) must failWith("at least one element matches '[n-o]' in 'List(one, two)'")
      expectation(List("one", "two") aka "the list" must notContainMatch("[n-o]")) must failWith("at least one element matches '[n-o]' in the list 'List(one, two)'")
    }
    "provide a 'must containMatchOnlyOnce' matcher on iterables checking if there is only one match" in {
      List("one", "three") must containMatchOnlyOnce("[n-o]")
      expectation(List("one", "two") must containMatchOnlyOnce("[n-o]")) must failWith("more than one element matches '[n-o]' in 'List(one, two)'")
    }
    "provide a 'haveSize' matcher checking the size of a collection" in {
      List("one", "two") must haveSize(2)
      expectation(List("one", "two") must haveSize(3)) must failWith("'List(one, two)' doesn't have size 3. It has size 2")
      expectation(List("one", "two") aka "the list" must haveSize(3)) must failWith("the list 'List(one, two)' doesn't have size 3. It has size 2")
    }
    "provide a 'must be empty' matcher on iterables" in {
      List() must be empty
    }
    "provide a 'must not be empty' matcher on iterables" in {
      List("1") must not be empty
    }
    "provide a 'must not contain' matcher on iterables" in {
      List("one", "two") must not contain("three")
    }
    "provide a 'must not containMatch' matcher on iterables" in {
      List("one", "two") must not containMatch("z.*")
    }
    "provide a 'must not have' matcher on iterables" in {
      List("one", "two") must not have((_:String).contains("z"))
    }
    "provide a 'have size' matcher checking the size of a collection" in {
      List("one", "two") must have size(2)
    }
    "provide a 'must be empty' matcher on any type of iterable" in {
      (Nil:Iterable[Int]) must be empty
      val list: List[Int] = Nil
      list must be empty
      val iterable: Iterable[Int] = Nil
      iterable must be empty
      val seq: Seq[Int] = Nil
      seq must be empty
      val map: Map[Int, Int] = Map()
      map must be empty
      val set: Set[String] = Set("one")
      set must not contain("two")
      val array: Array[String] = List("one").toArray
      array must not contain("two")
    }
    "provide a 'be the sameSetAs' matcher" in {
      Set("1") must be the sameSetAs(Set("1"))
    }
    "provide a 'be the sameSeqAs' matcher" in {
      "on empty seqs" >> {
        List[Int]() must be the sameSeqAs(List[Int]())
      }
      "on non-empty seqs" >> {
        List(1) must be the sameSeqAs(List(1))
      }
      "on seqs of different sizes" >> {
        expectation(List(1) must beTheSameSeqAs(List(1, 2))) must failWith("List(1) doesn't have the same size as List(1, 2)")
        expectation(List(1, 2) must beTheSameSeqAs(List())) must failWith("List(1, 2) doesn't have the same size as List()")
      }
    }
  }
}
