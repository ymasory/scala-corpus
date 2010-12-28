/*
 * Copyright 2009 Twitter, Inc.
 * Copyright 2009 Robey Pointer <robeypointer@gmail.com>
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


object KeyHasherSpec extends Specification {

  "KeyHasher" should {
    val longKey = "the easter island statue memorial tabernacle choir presents"

    "FNV1-32" in {
      KeyHasher.FNV1_32.hashKey("".getBytes) mustEqual 2166136261L
      KeyHasher.FNV1_32.hashKey("\uffff".getBytes("utf-8")) mustEqual 4055256578L
      KeyHasher.FNV1_32.hashKey("cat".getBytes) mustEqual 983016379L
      KeyHasher.FNV1_32.hashKey(longKey.getBytes) mustEqual 2223726839L
    }

    "FNV1A-32" in {
      KeyHasher.FNV1A_32.hashKey("".getBytes) mustEqual 2166136261L
      KeyHasher.FNV1A_32.hashKey("\uffff".getBytes("utf-8")) mustEqual 21469476L
      KeyHasher.FNV1A_32.hashKey("cat".getBytes) mustEqual 108289031L
      KeyHasher.FNV1A_32.hashKey(longKey.getBytes) mustEqual 1968151335L
    }

    "FNV1-64" in {
      KeyHasher.FNV1_64.hashKey("".getBytes) mustEqual 2216829733L
      KeyHasher.FNV1_64.hashKey("\uffff".getBytes("utf-8")) mustEqual 1779777890L
      KeyHasher.FNV1_64.hashKey("cat".getBytes) mustEqual 1806270427L
      KeyHasher.FNV1_64.hashKey(longKey.getBytes) mustEqual 3588698999L
    }

    "FNV1A-64" in {
      KeyHasher.FNV1A_64.hashKey("".getBytes) mustEqual 2216829733L
      KeyHasher.FNV1A_64.hashKey("\uffff".getBytes("utf-8")) mustEqual 2522373860L
      KeyHasher.FNV1A_64.hashKey("cat".getBytes) mustEqual 216310567L
      KeyHasher.FNV1A_64.hashKey(longKey.getBytes) mustEqual 2969891175L
    }

    "ketama" in {
      KeyHasher.KETAMA.hashKey("".getBytes) mustEqual 3649838548L
      KeyHasher.KETAMA.hashKey("\uffff".getBytes("utf-8")) mustEqual 844455094L
      KeyHasher.KETAMA.hashKey("cat".getBytes) mustEqual 1156741072L
      KeyHasher.KETAMA.hashKey(longKey.getBytes) mustEqual 3103958980L
    }

    "CRC32-ITU" in {
      KeyHasher.CRC32_ITU.hashKey("".getBytes) mustEqual 0L
      KeyHasher.CRC32_ITU.hashKey("\uffff".getBytes("utf-8")) mustEqual 1702646501L
      KeyHasher.CRC32_ITU.hashKey("cat".getBytes) mustEqual 2656977832L
      KeyHasher.CRC32_ITU.hashKey(longKey.getBytes) mustEqual 1410673605L
    }
  }
}
