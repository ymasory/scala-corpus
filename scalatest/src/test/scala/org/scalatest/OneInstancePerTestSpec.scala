/*
 * Copyright 2001-2009 Artima, Inc.
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
package org.scalatest

import events._

class TopLevelSuite extends Suite with OneInstancePerTest {
  import TopLevelSuite.sideEffectWasNotSeen
  var sideEffectWasIsolated = true
  def testOne() {
    sideEffectWasNotSeen = sideEffectWasNotSeen && sideEffectWasIsolated
    sideEffectWasIsolated = false
  }
  def testTwo() {
    sideEffectWasNotSeen = sideEffectWasNotSeen && sideEffectWasIsolated
    sideEffectWasIsolated = false
  }
  def testThree() {
    sideEffectWasNotSeen = sideEffectWasNotSeen && sideEffectWasIsolated
    sideEffectWasIsolated = false
  }
}
object TopLevelSuite {
  var sideEffectWasNotSeen = true
}

class OneInstancePerTestSpec extends Spec with SharedHelpers {
  describe("The OneInstancePerTest trait") {
    it("should isolate side effects from one test to the next in a top level Suite class that does not override newInstance") {
      var sideEffectWasNotSeen = true
      class MySuite extends Suite with OneInstancePerTest {
        var sideEffectWasIsolated = true
        def testOne() {
          sideEffectWasNotSeen = sideEffectWasNotSeen && sideEffectWasIsolated
          sideEffectWasIsolated = false
        }
        def testTwo() {
          sideEffectWasNotSeen = sideEffectWasNotSeen && sideEffectWasIsolated
          sideEffectWasIsolated = false
        }
        def testThree() {
          sideEffectWasNotSeen = sideEffectWasNotSeen && sideEffectWasIsolated
          sideEffectWasIsolated = false
        }
        override def newInstance = new MySuite
      }
      val suite = new MySuite
      suite.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(sideEffectWasNotSeen)
    }
    it("should isolate side effects from one test to the next in an inner Suite class that overrides newInstance") {
      val suite = new TopLevelSuite
      suite.run(None, SilentReporter, new Stopper {}, Filter(), Map(), None, new Tracker())
      assert(TopLevelSuite.sideEffectWasNotSeen)
    }
  }
}
