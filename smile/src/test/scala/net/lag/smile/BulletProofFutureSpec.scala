/*
 * Copyright 2009 Twitter, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.lag.smile

import org.specs._

object BulletProofFutureSpec extends Specification {
  "BulletProofFuture" should {
    "behave like a normal future" in {
      val future = BulletProofFuture.future { 37 - 14 }
      future() mustEqual 23
    }

    "re-throw an exception" in {
      val future = BulletProofFuture.future { 37 - "x14".toInt }
      future() must throwA[NumberFormatException]
    }
  }
}
