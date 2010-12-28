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

import net.lag.extensions._
import scala.collection.mutable


/**
 * Locate a node by taking the mod of the key's hash against the number of servers. This is
 * the default memcache node locator.
 */
class ModuloNodeLocator(hasher: KeyHasher) extends NodeLocator {

  def this() = this(KeyHasher.CRC32_ITU)

  var pool: ServerPool = null
  var continuum: Array[MemcacheConnection] = null

  def setPool(pool: ServerPool) = {
    this.pool = pool
    val stack = new mutable.ListBuffer[MemcacheConnection]
    for (s <- pool.liveServers) {
      for (i <- 1 to s.weight) {
        stack += s
      }
    }
    continuum = stack.toArray
  }

  /**
   * Return the server node that should contain this key.
   */
  def findNode(key: Array[Byte]): MemcacheConnection = {
    val index = (hasher.hashKey(key) % continuum.size).toInt
    continuum(index)
  }

  override def toString() = {
    "<RoundRobinNodeLocator hash=%s>".format(hasher)
  }
}
