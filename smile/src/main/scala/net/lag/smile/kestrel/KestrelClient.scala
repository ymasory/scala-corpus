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

package net.lag.smile.kestrel

import net.lag.configgy.ConfigMap
import net.lag.logging.Logger
import scala.collection.mutable
import scala.collection.immutable


/**
 * Kestrel cluster, configured by the config file.
 */
class KestrelClient(val messageStore: MessageStore) {
  val log = Logger.get
  @volatile var stopFlag = false

  /**
   * Underlying message store implementation, which may be replaced during unit tests.
   */
  var impl: MessageStore = messageStore


  def this(config: ConfigMap) = this(if (config.getBool("mock", false)) new MemoryStore else new MemcacheStore(config))


  /**
   * Returns an item if one is immediately available from the first kestrel contacted.
   * If not, returns None immediately.
   */
  def poll(key: String): Option[String] = impl.poll(key)

  /**
   * Returns an item if one is immediately available from the first kestrel contacted.
   * If not, returns None immediately.
   */
  def pollData(key: String): Option[Array[Byte]] = impl.pollData(key)

  /**
   * Waits for an item on this queue, with no timeout. The operation may be aborted
   * externally via `stop`.
   */
  def get(key: String): Option[String] = get(key, 0)

  /**
   * Waits for an item on this queue, with no timeout. The operation may be aborted
   * externally via `stop`.
   */
  def getData(key: String): Option[Array[Byte]] = getData(key, 0)

  /**
   * Waits for an item on this queue, for up to `timeoutMsec` milliseconds.
   * The operation may be aborted externally via `stop`.
   */
  def get(key: String, timeoutMsec: Int): Option[String] = {
    val expire = if (timeoutMsec > 0) {
      System.currentTimeMillis + timeoutMsec
    } else {
      Math.MAX_LONG
    }
    getExpire(key, expire, (key) => impl.poll(key))
  }

  /**
   * Waits for an item on this queue, for up to `timeoutMsec` milliseconds.
   * The operation may be aborted externally via `stop`.
   */
  def getData(key: String, timeoutMsec: Int): Option[Array[Byte]] = {
    val expire = if (timeoutMsec > 0) {
      System.currentTimeMillis + timeoutMsec
    } else {
      Math.MAX_LONG
    }
    getExpire(key, expire, (key) => impl.pollData(key))
  }

  /**
   * Waits for an item on this queue, until the current (java) clock time is after
   * `expire`.
   */
  protected def getExpire[T](key: String, expire: Long, pollMethod: (String) => Option[T]): Option[T] = {
    var rv: Option[T] = None
    var round = 0
    // originally these were 2, 1000, and 1.2, but i think that's too long and too gradual.
    // use all longs here to avoid overflow when MAX_LONG is passed in
    var sleepMs = 10L
    val SLEEPMAXMS = 250L
    val SLEEPMINMS = 1L
    val SLEEPMULTIPLICAND = 1.5

    while (rv == None && !stopFlag) {
      try {
        rv = pollMethod(key)
        if (rv == None) {
          val left = expire - System.currentTimeMillis
          if (left < 0) {
            return None
          }
          if (round == 0) {
            log.trace("waiting on kestrel queue: %s", key)
          }
          round += 1
          sleepMs = (SLEEPMAXMS min (sleepMs * SLEEPMULTIPLICAND).toLong min left) max SLEEPMINMS
          Thread.sleep(sleepMs)
        }
      } catch {
        case e: InterruptedException =>
          log.debug("Interrupted while talking to kestrel (get): %s -- rethrow", e)
          throw e
        case e: Exception =>
          log.warning("Exception talking to kestrel (get): %s -- retrying", e)
          Thread.sleep(250)
      }
    }

    rv
  }

  /**
   * Put an item on a queue.
   */
  def put(key: String, value: String): Unit = {
    try {
      impl.put(key, value)
    } catch {
      case e: Exception =>
        log.warning("Exception talking to kestrel (set): %s -- retrying", e)
        Thread.sleep(250)
        // if it fails this time, intentionally throw the exception.
        impl.put(key, value)
    }
  }

  /**
   * Put an item on a queue with a given expiry.
   */
  def put(key: String, value: String, expiry: Int): Unit = {
    try {
      impl.put(key, value, expiry)
    } catch {
      case e: Exception =>
        log.warning("Exception talking to kestrel (set): %s -- retrying", e)
        Thread.sleep(250)
        // if it fails this time, intentionally throw the exception.
        impl.put(key, value, expiry)
    }
  }

  /**
   * Get the stats.
   */
  def stats(): List[(String, Map[String, String])] = {
    stopFlag = false
    impl.stats
  }

  /**
   * FIXME remove.
   */
  @deprecated
  def queueIterator[T](key: String)(unpacker: String => T) = new Iterator[T] {
    var nextItem: Option[String] = None

    def hasNext = {
      nextItem = get(key)
      nextItem != None
    }

    def next() = nextItem.map(unpacker).get
  }

  /**
   * Shutdown the client, prematurely terminating any present and future `get` requests.
   */
  def shutdown() = {
    stopFlag = true
    impl.shutdown
  }

  /**
   * Reset the kestrel client to a known "fresh" state, for unit tests.
   */
  def reset(impl: MessageStore) = {
    stopFlag = false
    this.impl = impl
  }

  /**
   * Pause the client, prematurely terminating any present and future `get` requests. It may be
   * resumed later with `resume`. This has no effect on `put` requests.
   */
  def pause() = {
    stopFlag = true
  }

  /**
   * Resume the client, allowing `get` requests.
   */
  def resume() = {
    stopFlag = false
  }
}
