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

import java.util.Random
import scala.collection.mutable
import com.twitter.xrayspecs.Time
import net.lag.extensions._


/**
 * Choose the next node, a true round-robin. Useful for communicating with a cluster of
 * Kestrel queues. Node order is randomized, in part to support weights.
 */
class SequentialNodeLocator(hasher: KeyHasher) extends NodeLocator {

  // KeyHasher is unused, but required by superclass
  def this() = this(KeyHasher.CRC32_ITU)

  var pool: ServerPool = null
  var continuum: Array[MemcacheConnection] = null
  var count = 0
  var current = 0

  def setPool(pool: ServerPool) = {
    this.pool = pool
    val fanout = new mutable.ArrayBuffer[MemcacheConnection]
    for (s <- pool.liveServers) {
      for (i <- 1 to s.weight) {
        fanout += s
      }
    }

    val rand = new Random(Time.now.inMilliseconds)
    val randomized = new mutable.ListBuffer[MemcacheConnection]
    while (fanout.size > 0) {
      val idx = rand.nextInt(fanout.size)
      randomized += fanout(idx)
      fanout.remove(idx)
    }

    continuum = randomized.toArray
    count = continuum.size
  }

  /**
   * Return the server node that should contain this key.
   */
  def findNode(key: Array[Byte]): MemcacheConnection = {
    this.synchronized {
      val rv = continuum(current)
      current = (current + 1) % count
      rv
    }
  }

  override def toString() = {
    "<SequentialNodeLocator hash=null>"
  }
}
