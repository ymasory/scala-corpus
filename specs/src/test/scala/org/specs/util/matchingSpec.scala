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
package org.specs.util
import Matching.bestMatch
import org.specs.collection.ExtendedList.listToExtendedList
import scala.math._
import org.scalacheck.{ Prop, Gen }
import org.specs._

class matchingSpec extends SpecificationWithJUnit with ScalaCheck {
  var edgeFunction = (t:(String, String)) => t
  var edgeWeight = (t:(String, String)) => t._1.size + t._2.size 
  val sets = for {
    size1   <- Gen.choose(1, 3)
    set1    <- Gen.listOfN(size1, Gen.oneOf("Ar", "Bill", "Charles"))
    size2   <- Gen.choose(1, 3)
    set2   <- Gen.listOfN(size2, Gen.oneOf("An", "Bess", "Claris"))
  } yield (set1, set2) 

  "matching an empty set with an empty set returns an empty list" in {
    bestMatch(List[String](), List[String](), edgeFunction, edgeWeight) must be empty
  }
  "matching 2 non-empty sets must return edges with the maximum sum of weigths" in {
    // this property won't pass until a less naive algorithm is implemented
    sets must pass { s: (Seq[String], Seq[String]) => val (set1, set2) = s
      val maxOfSet1 = set1.toList.maximum((_:String).size)
      val maxOfSet2 = set2.toList.maximum((_:String).size)
     true// bestMatch(set1, set2, edgeFunction, edgeWeight).map(_._3).foldLeft(0)((total, t) => total + edgeWeight(t)) must >=(maxOfSet1 + maxOfSet2)
    }
  }
  "matching 2 non-empty sets must return a list of edges which size is the minimum size of both sets" in {
    sets must pass { s: (Seq[String], Seq[String]) => val (set1, set2) = s
      bestMatch(set1, set2, edgeFunction, edgeWeight) must have size(min(set1.size, set2.size))
    }
  }
  "matching a set with duplicated element" in {
    val set1 = List("Art", "Art")
    val set2 = List("Art", "Bill")
    edgeWeight = (t:(String, String)) => t._1.size +  t._2.size 
    bestMatch(set1, set2, edgeFunction, edgeWeight).toString must include("Art") and include("Bill")
  }
}
