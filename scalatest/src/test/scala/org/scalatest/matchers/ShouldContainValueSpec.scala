/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.matchers

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._

class ShouldContainValueSpec extends Spec with ShouldMatchers with Checkers with ReturnsNormallyThrowsAssertion {

  // Checking for a specific size
  describe("The 'contain value (Int)' syntax") {

    describe("on scala.collection.immutable.Map") {

      it("should do nothing if map contains specified value") {
        Map("one" -> 1, "two" -> 2) should contain value (2)
        Map("one" -> 1, "two" -> 2) should (contain value (2))
        Map(1 -> "one", 2 -> "two") should contain value ("two")
      }

      it("should do nothing if map does not contain the specified value and used with not") {
        Map("one" -> 1, "two" -> 2) should not { contain value (3) }
        Map("one" -> 1, "two" -> 2) should not contain value (3)
        Map("one" -> 1, "two" -> 2) should (not contain value (3))
      }

      it("should do nothing when map contains specified value and used in a logical-and expression") {
        Map("one" -> 1, "two" -> 2) should { contain value (2) and (contain value (1)) }
        Map("one" -> 1, "two" -> 2) should ((contain value (2)) and (contain value (1)))
        Map("one" -> 1, "two" -> 2) should (contain value (2) and contain value (1))
      }

      it("should do nothing when map contains specified value and used in a logical-or expression") {
        Map("one" -> 1, "two" -> 2) should { contain value (7) or (contain value (1)) }
        Map("one" -> 1, "two" -> 2) should ((contain value (7)) or (contain value (1)))
        Map("one" -> 1, "two" -> 2) should (contain value (7) or contain value (1))
      }

      it("should do nothing when map does not contain the specified value and used in a logical-and expression with not") {
        Map("one" -> 1, "two" -> 2) should { not { contain value (5) } and not { contain value (3) }}
        Map("one" -> 1, "two" -> 2) should ((not contain value (5)) and (not contain value (3)))
        Map("one" -> 1, "two" -> 2) should (not contain value (5) and not contain value (3))
      }

      it("should do nothing when map does not contain the specified value and used in a logical-or expression with not") {
        Map("one" -> 1, "two" -> 2) should { not { contain value (2) } or not { contain value (3) }}
        Map("one" -> 1, "two" -> 2) should ((not contain value (2)) or (not contain value (3)))
        Map("one" -> 1, "two" -> 2) should (not contain value (2) or not contain value (3))
      }

      it("should throw TestFailedException if map does not contain the specified value") {
        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should contain value (3)
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 3")
      }

      it("should throw TestFailedException if contains the specified value when used with not") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (not contain value (2))
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should not (contain value (2))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should not contain value (2)
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained value 2")
      }

      it("should throw an TestFailedException when map doesn't contain specified value and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { contain value (5) and (contain value (2)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((contain value (5)) and (contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (contain value (5) and contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")
      }

      it("should throw an TestFailedException when map doesn't contain specified value and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { contain value (55) or (contain value (22)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((contain value (55)) or (contain value (22)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (contain value (55) or contain value (22))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")
      }

      it("should throw an TestFailedException when map contains specified value and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { not { contain value (3) } and not { contain value (2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((not contain value (3)) and (not contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (not contain value (3) and not contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")
      }

      it("should throw an TestFailedException when map contains specified value and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { not { contain value (2) } or not { contain value (2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((not contain value (2)) or (not contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (not contain value (2) or not contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")
      }
    }

    describe("on scala.collection.mutable.Map") {

      import scala.collection.mutable

      it("should do nothing if map contains specified value") {
        mutable.Map("one" -> 1, "two" -> 2) should contain value (2)
        mutable.Map("one" -> 1, "two" -> 2) should (contain value (2))
        mutable.Map(1 -> "one", 2 -> "two") should contain value ("two")
      }

      it("should do nothing if map does not contain the specified value and used with not") {
        mutable.Map("one" -> 1, "two" -> 2) should not { contain value (3) }
        mutable.Map("one" -> 1, "two" -> 2) should not contain value (3)
        mutable.Map("one" -> 1, "two" -> 2) should (not contain value (3))
      }

      it("should do nothing when map contains specified value and used in a logical-and expression") {
        mutable.Map("one" -> 1, "two" -> 2) should { contain value (2) and (contain value (1)) }
        mutable.Map("one" -> 1, "two" -> 2) should ((contain value (2)) and (contain value (1)))
        mutable.Map("one" -> 1, "two" -> 2) should (contain value (2) and contain value (1))
      }

      it("should do nothing when map contains specified value and used in a logical-or expression") {
        mutable.Map("one" -> 1, "two" -> 2) should { contain value (7) or (contain value (1)) }
        mutable.Map("one" -> 1, "two" -> 2) should ((contain value (7)) or (contain value (1)))
        mutable.Map("one" -> 1, "two" -> 2) should (contain value (7) or contain value (1))
      }

      it("should do nothing when map does not contain the specified value and used in a logical-and expression with not") {
        mutable.Map("one" -> 1, "two" -> 2) should { not { contain value (5) } and not { contain value (3) }}
        mutable.Map("one" -> 1, "two" -> 2) should ((not contain value (5)) and (not contain value (3)))
        mutable.Map("one" -> 1, "two" -> 2) should (not contain value (5) and not contain value (3))
      }

      it("should do nothing when map does not contain the specified value and used in a logical-or expression with not") {
        mutable.Map("one" -> 1, "two" -> 2) should { not { contain value (2) } or not { contain value (3) }}
        mutable.Map("one" -> 1, "two" -> 2) should ((not contain value (2)) or (not contain value (3)))
        mutable.Map("one" -> 1, "two" -> 2) should (not contain value (2) or not contain value (3))
      }

      it("should throw TestFailedException if map does not contain the specified value") {
        val caught1 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should contain value (3)
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 3")
      }

      it("should throw TestFailedException if contains the specified value when used with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should (not contain value (2))
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should not (contain value (2))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should not contain value (2)
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained value 2")
      }

      it("should throw an TestFailedException when map doesn't contain specified value and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should { contain value (5) and (contain value (2)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")

        val caught2 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should ((contain value (5)) and (contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")

        val caught3 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should (contain value (5) and contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")
      }

      it("should throw an TestFailedException when map doesn't contain specified value and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should { contain value (55) or (contain value (22)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")

        val caught2 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should ((contain value (55)) or (contain value (22)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")

        val caught3 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should (contain value (55) or contain value (22))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")
      }

      it("should throw an TestFailedException when map contains specified value and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should { not { contain value (3) } and not { contain value (2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should ((not contain value (3)) and (not contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should (not contain value (3) and not contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")
      }

      it("should throw an TestFailedException when map contains specified value and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should { not { contain value (2) } or not { contain value (2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should ((not contain value (2)) or (not contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          mutable.Map("one" -> 1, "two" -> 2) should (not contain value (2) or not contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")
      }
    }

    describe("on scala.collection.Map") {

      val map: scala.collection.Map[String, Int] = Map("one" -> 1, "two" -> 2)

      it("should do nothing if map contains specified value") {
        map should contain value (2)
        map should (contain value (2))
      }

      it("should do nothing if map does not contain the specified value and used with not") {
        map should not { contain value (3) }
        map should not contain value (3)
        map should (not contain value (3))
      }

      it("should do nothing when map contains specified value and used in a logical-and expression") {
        map should { contain value (2) and (contain value (1)) }
        map should ((contain value (2)) and (contain value (1)))
        map should (contain value (2) and contain value (1))
      }

      it("should do nothing when map contains specified value and used in a logical-or expression") {
        map should { contain value (7) or (contain value (1)) }
        map should ((contain value (7)) or (contain value (1)))
        map should (contain value (7) or contain value (1))
      }

      it("should do nothing when map does not contain the specified value and used in a logical-and expression with not") {
        map should { not { contain value (5) } and not { contain value (3) }}
        map should ((not contain value (5)) and (not contain value (3)))
        map should (not contain value (5) and not contain value (3))
      }

      it("should do nothing when map does not contain the specified value and used in a logical-or expression with not") {
        map should { not { contain value (2) } or not { contain value (3) }}
        map should ((not contain value (2)) or (not contain value (3)))
        map should (not contain value (2) or not contain value (3))
      }

      it("should throw TestFailedException if map does not contain the specified value") {
        val caught1 = intercept[TestFailedException] {
          map should contain value (3)
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 3")
      }

      it("should throw TestFailedException if contains the specified value when used with not") {

        val caught1 = intercept[TestFailedException] {
          map should (not contain value (2))
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          map should not (contain value (2))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          map should not contain value (2)
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained value 2")
      }

      it("should throw an TestFailedException when map doesn't contain specified value and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          map should { contain value (5) and (contain value (2)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")

        val caught2 = intercept[TestFailedException] {
          map should ((contain value (5)) and (contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")

        val caught3 = intercept[TestFailedException] {
          map should (contain value (5) and contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")
      }

      it("should throw an TestFailedException when map doesn't contain specified value and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          map should { contain value (55) or (contain value (22)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")

        val caught2 = intercept[TestFailedException] {
          map should ((contain value (55)) or (contain value (22)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")

        val caught3 = intercept[TestFailedException] {
          map should (contain value (55) or contain value (22))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")
      }

      it("should throw an TestFailedException when map contains specified value and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          map should { not { contain value (3) } and not { contain value (2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          map should ((not contain value (3)) and (not contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          map should (not contain value (3) and not contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")
      }

      it("should throw an TestFailedException when map contains specified value and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          map should { not { contain value (2) } or not { contain value (2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          map should ((not contain value (2)) or (not contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          map should (not contain value (2) or not contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")
      }
    }

    describe("on scala.collection.immutable.HashMap") {

      import scala.collection.immutable.HashMap

      it("should do nothing if map contains specified value") {
        HashMap("one" -> 1, "two" -> 2) should contain value (2)
        HashMap("one" -> 1, "two" -> 2) should (contain value (2))
        HashMap(1 -> "one", 2 -> "two") should contain value ("two")
      }

      it("should do nothing if map does not contain the specified value and used with not") {
        HashMap("one" -> 1, "two" -> 2) should not { contain value (3) }
        HashMap("one" -> 1, "two" -> 2) should not contain value (3)
        HashMap("one" -> 1, "two" -> 2) should (not contain value (3))
      }

      it("should do nothing when map contains specified value and used in a logical-and expression") {
        HashMap("one" -> 1, "two" -> 2) should { contain value (2) and (contain value (1)) }
        HashMap("one" -> 1, "two" -> 2) should ((contain value (2)) and (contain value (1)))
        HashMap("one" -> 1, "two" -> 2) should (contain value (2) and contain value (1))
      }

      it("should do nothing when map contains specified value and used in a logical-or expression") {
        HashMap("one" -> 1, "two" -> 2) should { contain value (7) or (contain value (1)) }
        HashMap("one" -> 1, "two" -> 2) should ((contain value (7)) or (contain value (1)))
        HashMap("one" -> 1, "two" -> 2) should (contain value (7) or contain value (1))
      }

      it("should do nothing when map does not contain the specified value and used in a logical-and expression with not") {
        HashMap("one" -> 1, "two" -> 2) should { not { contain value (5) } and not { contain value (3) }}
        HashMap("one" -> 1, "two" -> 2) should ((not contain value (5)) and (not contain value (3)))
        HashMap("one" -> 1, "two" -> 2) should (not contain value (5) and not contain value (3))
      }

      it("should do nothing when map does not contain the specified value and used in a logical-or expression with not") {
        HashMap("one" -> 1, "two" -> 2) should { not { contain value (2) } or not { contain value (3) }}
        HashMap("one" -> 1, "two" -> 2) should ((not contain value (2)) or (not contain value (3)))
        HashMap("one" -> 1, "two" -> 2) should (not contain value (2) or not contain value (3))
      }

      it("should throw TestFailedException if map does not contain the specified value") {
        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should contain value (3)
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 3")
      }

      it("should throw TestFailedException if contains the specified value when used with not") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (not contain value (2))
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should not (contain value (2))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should not contain value (2)
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained value 2")
      }

      it("should throw an TestFailedException when map doesn't contain specified value and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { contain value (5) and (contain value (2)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((contain value (5)) and (contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (contain value (5) and contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")
      }

      it("should throw an TestFailedException when map doesn't contain specified value and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { contain value (55) or (contain value (22)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((contain value (55)) or (contain value (22)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (contain value (55) or contain value (22))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")
      }

      it("should throw an TestFailedException when map contains specified value and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { not { contain value (3) } and not { contain value (2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((not contain value (3)) and (not contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (not contain value (3) and not contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")
      }

      it("should throw an TestFailedException when map contains specified value and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { not { contain value (2) } or not { contain value (2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((not contain value (2)) or (not contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (not contain value (2) or not contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")
      }
    }

    describe("on scala.collection.mutable.HashMap") {

      import scala.collection.mutable

      it("should do nothing if map contains specified value") {
        mutable.HashMap("one" -> 1, "two" -> 2) should contain value (2)
        mutable.HashMap("one" -> 1, "two" -> 2) should (contain value (2))
        mutable.HashMap(1 -> "one", 2 -> "two") should contain value ("two")
      }

      it("should do nothing if map does not contain the specified value and used with not") {
        mutable.HashMap("one" -> 1, "two" -> 2) should not { contain value (3) }
        mutable.HashMap("one" -> 1, "two" -> 2) should not contain value (3)
        mutable.HashMap("one" -> 1, "two" -> 2) should (not contain value (3))
      }

      it("should do nothing when map contains specified value and used in a logical-and expression") {
        mutable.HashMap("one" -> 1, "two" -> 2) should { contain value (2) and (contain value (1)) }
        mutable.HashMap("one" -> 1, "two" -> 2) should ((contain value (2)) and (contain value (1)))
        mutable.HashMap("one" -> 1, "two" -> 2) should (contain value (2) and contain value (1))
      }

      it("should do nothing when map contains specified value and used in a logical-or expression") {
        mutable.HashMap("one" -> 1, "two" -> 2) should { contain value (7) or (contain value (1)) }
        mutable.HashMap("one" -> 1, "two" -> 2) should ((contain value (7)) or (contain value (1)))
        mutable.HashMap("one" -> 1, "two" -> 2) should (contain value (7) or contain value (1))
      }

      it("should do nothing when map does not contain the specified value and used in a logical-and expression with not") {
        mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain value (5) } and not { contain value (3) }}
        mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain value (5)) and (not contain value (3)))
        mutable.HashMap("one" -> 1, "two" -> 2) should (not contain value (5) and not contain value (3))
      }

      it("should do nothing when map does not contain the specified value and used in a logical-or expression with not") {
        mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain value (2) } or not { contain value (3) }}
        mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain value (2)) or (not contain value (3)))
        mutable.HashMap("one" -> 1, "two" -> 2) should (not contain value (2) or not contain value (3))
      }

      it("should throw TestFailedException if map does not contain the specified value") {
        val caught1 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should contain value (3)
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 3")
      }

      it("should throw TestFailedException if contains the specified value when used with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should (not contain value (2))
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should not (contain value (2))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should not contain value (2)
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained value 2")
      }

      it("should throw an TestFailedException when map doesn't contain specified value and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should { contain value (5) and (contain value (2)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")

        val caught2 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should ((contain value (5)) and (contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")

        val caught3 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should (contain value (5) and contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 5")
      }

      it("should throw an TestFailedException when map doesn't contain specified value and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should { contain value (55) or (contain value (22)) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")

        val caught2 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should ((contain value (55)) or (contain value (22)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")

        val caught3 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should (contain value (55) or contain value (22))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 55, and Map(one -> 1, two -> 2) did not contain value 22")
      }

      it("should throw an TestFailedException when map contains specified value and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain value (3) } and not { contain value (2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain value (3)) and (not contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should (not contain value (3) and not contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain value 3, but Map(one -> 1, two -> 2) contained value 2")
      }

      it("should throw an TestFailedException when map contains specified value and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain value (2) } or not { contain value (2) }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")

        val caught2 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain value (2)) or (not contain value (2)))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")

        val caught3 = intercept[TestFailedException] {
          mutable.HashMap("one" -> 1, "two" -> 2) should (not contain value (2) or not contain value (2))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained value 2, and Map(one -> 1, two -> 2) contained value 2")
      }
    }

    describe("on java.util.Map") {

      val javaMap: java.util.Map[String, Int] = new java.util.HashMap
      javaMap.put("one",1)
      javaMap.put("two", 2)

      it("should do nothing if map contains specified value") {
        javaMap should contain value (2)
        javaMap should (contain value (2))
      }

      it("should do nothing if map does not contain the specified value and used with not") {
        javaMap should not { contain value (3) }
        javaMap should not contain value (3)
        javaMap should (not contain value (3))
      }

      it("should do nothing when map contains specified value and used in a logical-and expression") {
        javaMap should { contain value (2) and (contain value (1)) }
        javaMap should ((contain value (2)) and (contain value (1)))
        javaMap should (contain value (2) and contain value (1))
      }

      it("should do nothing when map contains specified value and used in a logical-or expression") {
        javaMap should { contain value (9) or (contain value (1)) }
        javaMap should ((contain value (9)) or (contain value (1)))
        javaMap should (contain value (9) or contain value (1))
      }

      it("should do nothing when map does not contain the specified value and used in a logical-and expression with not") {
        javaMap should { not { contain value (5) } and not { contain value (3) }}
        javaMap should ((not contain value (5)) and (not contain value (3)))
        javaMap should (not contain value (5) and not contain value (3))
      }

      it("should do nothing when map does not contain the specified value and used in a logical-or expression with not") {
        javaMap should { not { contain value (2) } or not { contain value (3) }}
        javaMap should ((not contain value (2)) or (not contain value (3)))
        javaMap should (not contain value (2) or not contain value (3))
      }

      it("should throw TestFailedException if map does not contain the specified value") {
        val caught1 = intercept[TestFailedException] {
          javaMap should contain value (3)
        }
        caught1.getMessage should (be === "{one=1, two=2} did not contain value 3" or
          be === "{two=2, one=1} did not contain value 3")
      }

      it("should throw TestFailedException if contains the specified value when used with not") {

        val caught1 = intercept[TestFailedException] {
          javaMap should (not contain value (2))
        }
        caught1.getMessage should (be === "{one=1, two=2} contained value 2" or
          be === "{two=2, one=1} contained value 2")

        val caught2 = intercept[TestFailedException] {
          javaMap should not (contain value (2))
        }
        caught2.getMessage should (be === "{one=1, two=2} contained value 2" or
          be === "{two=2, one=1} contained value 2")

        val caught3 = intercept[TestFailedException] {
          javaMap should not contain value (2)
        }
        caught3.getMessage should (be === "{one=1, two=2} contained value 2" or
          be === "{two=2, one=1} contained value 2")
      }

      it("should throw an TestFailedException when map doesn't contain specified value and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          javaMap should { contain value (5) and (contain value (2)) }
        }
        caught1.getMessage should (be === "{one=1, two=2} did not contain value 5" or
          be === "{two=2, one=1} did not contain value 5")

        val caught2 = intercept[TestFailedException] {
          javaMap should ((contain value (5)) and (contain value (2)))
        }
        caught2.getMessage should (be === "{one=1, two=2} did not contain value 5" or
          be === "{two=2, one=1} did not contain value 5")

        val caught3 = intercept[TestFailedException] {
          javaMap should (contain value (5) and contain value (2))
        }
        caught3.getMessage should (be === "{one=1, two=2} did not contain value 5" or
          be === "{two=2, one=1} did not contain value 5")
      }

      it("should throw an TestFailedException when map doesn't contain specified value and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          javaMap should { contain value (55) or (contain value (22)) }
        }
        caught1.getMessage should (be === "{one=1, two=2} did not contain value 55, and {one=1, two=2} did not contain value 22" or
          be === "{two=2, one=1} did not contain value 55, and {two=2, one=1} did not contain value 22")

        val caught2 = intercept[TestFailedException] {
          javaMap should ((contain value (55)) or (contain value (22)))
        }
        caught2.getMessage should (be === "{one=1, two=2} did not contain value 55, and {one=1, two=2} did not contain value 22" or
          be === "{two=2, one=1} did not contain value 55, and {two=2, one=1} did not contain value 22")

        val caught3 = intercept[TestFailedException] {
          javaMap should (contain value (55) or contain value (22))
        }
        caught3.getMessage should (be === "{one=1, two=2} did not contain value 55, and {one=1, two=2} did not contain value 22" or
          be === "{two=2, one=1} did not contain value 55, and {two=2, one=1} did not contain value 22")
      }

      it("should throw an TestFailedException when map contains specified value and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaMap should { not { contain value (3) } and not { contain value (2) }}
        }
        caught1.getMessage should (be === "{one=1, two=2} did not contain value 3, but {one=1, two=2} contained value 2" or
          be === "{two=2, one=1} did not contain value 3, but {two=2, one=1} contained value 2")

        val caught2 = intercept[TestFailedException] {
          javaMap should ((not contain value (3)) and (not contain value (2)))
        }
        caught2.getMessage should (be === "{one=1, two=2} did not contain value 3, but {one=1, two=2} contained value 2" or
          be === "{two=2, one=1} did not contain value 3, but {two=2, one=1} contained value 2")

        val caught3 = intercept[TestFailedException] {
          javaMap should (not contain value (3) and not contain value (2))
        }
        caught3.getMessage should (be === "{one=1, two=2} did not contain value 3, but {one=1, two=2} contained value 2" or
          be === "{two=2, one=1} did not contain value 3, but {two=2, one=1} contained value 2")
      }

      it("should throw an TestFailedException when map contains specified value and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaMap should { not { contain value (2) } or not { contain value (2) }}
        }
        caught1.getMessage should (be === "{one=1, two=2} contained value 2, and {one=1, two=2} contained value 2" or
          be === "{two=2, one=1} contained value 2, and {two=2, one=1} contained value 2")

        val caught2 = intercept[TestFailedException] {
          javaMap should ((not contain value (2)) or (not contain value (2)))
        }
        caught2.getMessage should (be === "{one=1, two=2} contained value 2, and {one=1, two=2} contained value 2" or
          be === "{two=2, one=1} contained value 2, and {two=2, one=1} contained value 2")

        val caught3 = intercept[TestFailedException] {
          javaMap should (not contain value (2) or not contain value (2))
        }
        caught3.getMessage should (be === "{one=1, two=2} contained value 2, and {one=1, two=2} contained value 2" or
          be === "{two=2, one=1} contained value 2, and {two=2, one=1} contained value 2")
      }
    }
  }
}
