/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package common {

import _root_.org.specs._
import _root_.net.liftweb.common.Box._
import _root_.org.specs.runner._
import _root_.org.specs.Sugar._
import _root_.org.specs.ScalaCheck
import _root_.org.scalacheck.Gen._
import _root_.org.scalacheck._
import _root_.org.scalacheck.Arbitrary._
import _root_.org.scalacheck.Prop.{forAll}


class BoxUnitTest extends Runner(BoxUnit) with JUnit
object BoxUnit extends Specification with BoxGen with ScalaCheck {
  "A Box equals method" should {
    "return true with comparing two identical Box messages" in {
      val equality = (c1: Box[Int], c2: Box[Int]) => (c1, c2) match {
        case (Empty, Empty) => c1 == c2
        case (Full(x), Full(y)) => (c1 == c2) == (x == y)
        case (Failure(m1, e1, l1), Failure(m2, e2, l2)) => (c1 == c2) == ((m1, e1, l1) == (m2, e2, l2))
        case _ => c1 != c2
      }
      forAll(equality) must pass
    }
    "return false with comparing one Full and another object" in {
      Full(1) must_!= "hello"
    }
    "return false with comparing one Empty and another object" in {
      Empty must_!= "hello"
    }
    "return false with comparing one Failure and another object" in {
      Failure("", Empty, Empty) must_!= "hello"
    }
  }
}
trait BoxGen {

  implicit def genThrowable: Arbitrary[Throwable] = Arbitrary[Throwable] {
    case object UserException extends Throwable
    value(UserException)
  }

  implicit def genBox[T](implicit a: Arbitrary[T]): Arbitrary[Box[T]] = Arbitrary[Box[T]] {
    frequency(
      (3, value(Empty)),
      (3, a.arbitrary.map(Full[T])),
      (1, genFailureBox)
    )
  }

  def genFailureBox: Gen[Failure] = for {
    msgLen <- choose(0, 4)
    msg <- listOfN(msgLen, alphaChar)
    exception <- value(Full(new Exception("")))
    chainLen <- choose(1, 5)
    chain <- frequency((1, listOfN(chainLen, genFailureBox)), (3, value(Nil)))
  } yield Failure(msg.mkString, exception, Box(chain.headOption))

}

}
}
