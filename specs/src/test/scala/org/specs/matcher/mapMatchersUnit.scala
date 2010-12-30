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
import org.scalacheck.Prop._

class mapMatchersUnit extends MatchersSpecification with PartialFunctionGen with ScalaCheck {
  "Map matchers" should {
    "not evaluate the expressions twice: haveKey" in {
      val map: Iterable[(String, Any)] = Map("" -> 1)
      haveKey("") must evalOnce(exp(map))
    }
    "not evaluate the expressions twice: haveValue" in {
      val map2: Iterable[(Any, Int)] = Map("" -> 1)
      haveValue(1) must evalOnce(exp(map2))
    }
    "not evaluate the expressions twice: havePair" in {
      val map3: Iterable[(String, Int)] = Map("" -> 1)
      havePair("" ->1) must evalOnce(exp(map3))
    }
  }
  "Partial Functions matchers" should {
    val f = new PartialFunction[Int, String] {
      def isDefinedAt(i: Int) = i % 2 == 0
      def apply(i: Int) = (i*2).toString
    }
    "provide a beDefinedAt matcher checking if a PartialFunction is defined at specific values" in {
      val beDefinedAtAValue = (f: PartialFunction[Int, String], list: List[Int]) => {
        beDefinedAt(list: _*)(f) match {
          case (true, _, _) => list forall {x:Int => f.isDefinedAt(x)}
          case (false, _, ko) => { list forall {x:Int => ko.contains(x.toString) || f.isDefinedAt(x)} }
        }
      }
      forAll(beDefinedAtAValue) must pass
    }
    "provide a beDefinedBy matcher checking if a PartialFunction is defined at specific values and returns appropriate results" in {
      val beDefinedByValueAndResult = (f: PartialFunction[Int, String], map: Map[Int, String]) => {
        beDefinedBy(map.toList : _*)(f) match {
          case (true, _, _) => map forall {x:(Int, String) => f.isDefinedAt(x._1) && f(x._1) == x._2}
          case (false, _, ko) => { map forall {x:(Int, String) => ko.contains(x.toString) || (f.isDefinedAt(x._1) && f(x._1) == x._2)} }
        }
      }
      forAll(beDefinedByValueAndResult) must pass
    }
  }
}
import org.scalacheck.Gen._
import org.scalacheck._
import Arbitrary._
import Gen._

trait PartialFunctionGen {
  implicit def listInt: Arbitrary[List[Int]] = Arbitrary[List[Int]] {
    for {length <- choose(1, 4)
         l <- listOfN(length, choose(1, 4))
    } yield l.toList
  }
  implicit def genPartialFunction: Arbitrary[PartialFunction[Int, String]] = Arbitrary[PartialFunction[Int, String]] {
    for {length <- choose(0, 4)
         keys <- listOfN(length, choose(1, 4))
         values <- listOfN(length, Arbitrary.arbitrary[String])
    } yield Map(keys.toList zip values.toList map {kv => kv._1 -> kv._2} : _*)
  }
  implicit def genMap: Arbitrary[Map[Int, String]] = Arbitrary[Map[Int, String]] {
    for {length <- choose(0, 4)
         keys <- listOfN(length, choose(1, 4))
         values <- listOfN(length, Arbitrary.arbitrary[String])
    } yield Map(keys.toList zip values.toList map {kv => kv._1 -> kv._2} : _*)
  }
}
