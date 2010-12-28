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

import scala.collection.mutable


/**
 * Codec for converting objects of type T to/from binary data for storage in a memcache
 * cluster.
 */
trait MemcacheCodec[T] {
  def encode(value: T): Array[Byte]
  def decode(data: Array[Byte]): T
}


/**
 * Some standard codecs.
 */
object MemcacheCodec {
  /**
   * The standard memcache codec, which stores strings in UTF-8.
   */
  val UTF8 = new MemcacheCodec[String] {
    def encode(value: String) = value.getBytes("utf-8")
    def decode(data: Array[Byte]) = new String(data, "utf-8")
  }
}
