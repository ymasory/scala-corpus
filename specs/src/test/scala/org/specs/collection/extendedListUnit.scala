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
package org.specs.collection
import org.specs.collection.ExtendedList._
import org.specs.util.Products._
import org.specs.runner._
import org.specs._

class extendedListUnit extends SpecificationWithJUnit with TestData {

  "A removeFirst function" should {
    "remove nothing if the list is empty" in {
      (Nil: List[String]).removeFirst(_ == "a") must_== Nil
    }
    "remove only the first element of a list satisfying the predicate" in {
      List("a", "b", "c", "b").removeFirst(_ == "b") must_== List("a", "c", "b")
    }
  }
  "A removeFirstSeq function" should {
    "remove nothing if the source list is empty" in {
      (Nil: List[String]).removeFirstSeq(List()) must_== Nil
    }
    "remove the first subsequence of the list matching a the sequence passed as a parameter" in {
      List("a", "b", "c", "b", "c").removeFirstSeq(List("a", "b")) must_== List("c", "b", "c")
    }
    "remove *only* the first subsequence" in {
      List("a", "b", "c", "b", "c").removeFirstSeq(List("b", "c")) must_== List("a", "b", "c")
    }
  }
  "A function returning every order of a list" should {
    "return a list with one permutations for a list with one element" in {
      everyOrder(("a")) must be like { case List(List("a")) => ok }
    }
    "return a list of 2 permutations for a list with two elements" in {
      everyOrder(("a", "b")) must be like { case List(List("a", "b"), List("b", "a")) => ok }
    }
    "return a list of 6 permutations for a list with 3 elements" in {
      everyOrder(("a", "b", "c")) must contain(List("a", "b", "c")) and
                                       contain(List("c", "b", "a")) and
                                       contain(List("c", "a", "b")) and
                                       contain(List("b", "c", "a")) and
                                       contain(List("b", "a", "c")) and
                                       contain(List("a", "c", "b"))
    }
  }
  "A function mixing an element with a list" should {
    "create 2 couples with a list of one element" in {
      mix("a", List("b")) must be like { case List(List("a", "b"), List("b", "a")) => ok }
    }
    "create 3 lists with a list of 2 elements" in {
	  1 must_== 1 
	  /* this case still crashes the compiler
	  mix("a", List("b", "c")) must be like { case List(List("a", "b", "c"),
                                                       List("b", "a", "c"),
                                                       List("b", "c", "a")) => ok }
	 */
    }
  }
  "A 'prefixes' function" should {
    "return a list with all prefixes of the given list" in {
      prefixesAndPrefix must pass { t: (List[Int], List[List[Int]], Seq[Int]) => val (list, prefixes, prefix) = t
        prefixes must (beEmpty or contain(prefix)).when(!prefix.isEmpty)
      }(set(maxSize->5))
    }
  }
  "A 'toMap' function" should {
    "create a Map from a list where the list elements are the keys and the values are set to a default value" in {
      Map(1 -> "for you", 2 -> "for you") must havePairs(1 -> "for you", 2 -> "for you")
    }
  }
  "A maxElement function" should {
    "return the maximum element of a list, according to a valuation function" in {
      List("a", "bb", "ccc").maxElement(_.size) must_== Some("ccc")
    }
  }
}
import org.specs.Sugar._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.specs.ScalaCheck

trait TestData extends Sugar with ScalaCheck { this: SpecificationWithJUnit =>
   val prefixesAndPrefix = for (list <- listOf(Gen.oneOf(1, 2, 3, 4));
                                n <- choose(0, list.size-1);
                                val prefix = list.take(n))
                                yield (list, list.prefixes, prefix)
}
