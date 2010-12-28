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


/**
 * Interface for a low-level message store, implemented by MemcacheStore for kestrel, or
 * MemoryStore for an in-memory test fixture.
 */
trait MessageStore {
  /**
   * Makes an attempt to fetch an item off a queue, and returns immediately.
   */
  def pollData(key: String): Option[Array[Byte]]

  /**
   * Makes an attempt to fetch an item off a queue, and returns immediately.
   */
  def poll(key: String): Option[String]

  /**
   * Puts an item on the queue.
   */
  def putData(key: String, value: Array[Byte]): Unit

  /**
   * Puts an item on the queue.
   */
  def put(key: String, value: String): Unit

  /**
   * Puts an item on the queue with the given expiry
   */
  def put(key: String, value: String, expiry: Int): Unit

  /**
   * Shutdown any underlying library.
   */
  def shutdown(): Unit

  /**
   * Get stats,
   */
  def stats(): List[(String, Map[String, String])]
}
