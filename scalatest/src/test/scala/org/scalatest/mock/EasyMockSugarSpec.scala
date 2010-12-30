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
package org.scalatest.mock

import org.scalatest._
import matchers.ShouldMatchers

class EasyMockSugarSpec extends FlatSpec with ShouldMatchers with SharedHelpers {
  "The EasyMockSugar trait's whenExecuting method" should
          "work with multiple mocks passed in" in {
    val a = new Suite with EasyMockSugar {
      def testThatShouldFail() {
        trait OneFish {
          def eat(food: String) = ()
        }
        trait TwoFish {
          def eat(food: String) = ()
        }
        val oneFishMock = mock[OneFish]
        val twoFishMock = mock[TwoFish]

        expecting {
          oneFishMock.eat("red fish")
          twoFishMock.eat("blue fish")
        }

        whenExecuting(oneFishMock, twoFishMock) {
          oneFishMock.eat("red fish")
          twoFishMock.eat("green fish")
        }
      }

      def testThatShouldSucceed() {
        trait OneFish {
          def eat(food: String) = ()
        }
        trait TwoFish {
          def eat(food: String) = ()
        }
        val oneFishMock = mock[OneFish]
        val twoFishMock = mock[TwoFish]

        expecting {
          oneFishMock.eat("red fish")
          twoFishMock.eat("blue fish")
        }

        whenExecuting(oneFishMock, twoFishMock) {
          oneFishMock.eat("red fish")
          twoFishMock.eat("blue fish")
        }
      }

      def testThatShouldFailWithClass() {
        class OneFish {
          def eat(food: String) = ()
        }
        class TwoFish {
          def eat(food: String) = ()
        }
        val oneFishMock = mock[OneFish]
        val twoFishMock = mock[TwoFish]

        expecting {
          oneFishMock.eat("red fish")
          twoFishMock.eat("blue fish")
        }

        whenExecuting(oneFishMock, twoFishMock) {
          oneFishMock.eat("red fish")
          twoFishMock.eat("green fish")
        }
      }

      def testThatShouldSucceedWithClass() {
        class OneFish {
          def eat(food: String) = ()
        }
        class TwoFish {
          def eat(food: String) = ()
        }
        val oneFishMock = mock[OneFish]
        val twoFishMock = mock[TwoFish]

        expecting {
          oneFishMock.eat("red fish")
          twoFishMock.eat("blue fish")
        }

        whenExecuting(oneFishMock, twoFishMock) {
          oneFishMock.eat("red fish")
          twoFishMock.eat("blue fish")
        }
      }

      def testThatShouldFailStrict() {
        class OneFish {
          def eat(food: String) = ()
          def burp(flavor: String) = ()
        }
        class TwoFish {
          def eat(food: String) = ()
        }
        val oneFishMock = strictMock[OneFish]
        val twoFishMock = strictMock[TwoFish]

        expecting {
          oneFishMock.eat("red fish")
          oneFishMock.burp("red fish")
          twoFishMock.eat("blue fish")
        }

        whenExecuting(oneFishMock, twoFishMock) {
          oneFishMock.burp("red fish")
          oneFishMock.eat("red fish")
          twoFishMock.eat("blue fish")
        }
      }

      def testThatShouldSucceedStrict() {
        class OneFish {
          def eat(food: String) = ()
          def burp(flavor: String) = ()
        }
        class TwoFish {
          def eat(food: String) = ()
        }
        val oneFishMock = strictMock[OneFish]
        val twoFishMock = strictMock[TwoFish]

        expecting {
          oneFishMock.eat("red fish")
          oneFishMock.burp("red fish")
          twoFishMock.eat("blue fish")
        }

        whenExecuting(oneFishMock, twoFishMock) {
          oneFishMock.eat("red fish")
          oneFishMock.burp("red fish")
          twoFishMock.eat("blue fish")
        }
      }

      def testThatShouldSucceedNice() {
        class OneFish {
          def eat(food: String) = ()
          def burp(flavor: String) = ()
        }
        class TwoFish {
          def eat(food: String) = ()
        }
        val oneFishMock = niceMock[OneFish]
        val twoFishMock = niceMock[TwoFish]

        expecting {
          oneFishMock.eat("red fish")
          twoFishMock.eat("blue fish")
        }

        whenExecuting(oneFishMock, twoFishMock) {
          oneFishMock.eat("red fish")
          oneFishMock.burp("red fish")
          twoFishMock.eat("blue fish")
        }
      }

      def testThatShouldFailNice() {
        class OneFish {
          def eat(food: String) = ()
          def burp(flavor: String) = ()
        }
        class TwoFish {
          def eat(food: String) = ()
        }
        val oneFishMock = niceMock[OneFish]
        val twoFishMock = niceMock[TwoFish]

        expecting {
          oneFishMock.eat("red fish")
          oneFishMock.burp("red fish")
          twoFishMock.eat("blue fish")
        }

        whenExecuting(oneFishMock, twoFishMock) {
          oneFishMock.eat("red fish")
          twoFishMock.eat("blue fish")
        }
      }
    }
    val rep = new EventRecordingReporter
    a.run(None, rep, new Stopper {}, Filter(), Map(), None, new Tracker)
    val tf = rep.testFailedEventsReceived
    tf.size should be === 4
    val ts = rep.testSucceededEventsReceived
    ts.size should be === 4
  }
}
