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

import scala.collection.mutable


/**
 * MessageStore implementation that just reads from and writes to in-memory queues.
 */
class MemoryStore extends MessageStore {
  var queues = new mutable.HashMap[String, mutable.Queue[Array[Byte]]]

  def pollData(key: String): Option[Array[Byte]] = synchronized {
    queues.get(key) match {
      case None => None
      case Some(queue) =>
        queue.dequeueFirst(_ => true) match {
          case None => {
            notifyAll // Wake waitForEmpty
            None
          }
          case x @ Some(entry) => {
            if (queue.isEmpty) notifyAll // Wake waitForEmpty
            x
          }
        }
    }
  }

  def putData(key: String, value: Array[Byte]): Unit = synchronized {
    queues.getOrElseUpdate(key, new mutable.Queue[Array[Byte]]).enqueue(value)
  }

  def poll(key: String): Option[String] = {
    pollData(key) match {
      case None => None
      case Some(data) => Some(new String(data, "UTF-8"))
    }
  }

  def put(key: String, value: String) = putData(key, value.getBytes("UTF-8"))
  
  // The memory store has no concept of an expiry time.
  def put(key: String, value: String, expiry: Int) = putData(key, value.getBytes("UTF-8"))

  def shutdown() = { }
  
  def stats(): List[(String, Map[String, String])] = { List(("localhost", Map.empty)) }

  /**
   * Contents of the queues, in an easily-verifiable string format.
   */
  override def toString = {
    (for (val key <- queues.keys.toList sort (_ < _))
     yield "%s=%s".format(key, queues(key).map { new String(_) }.toList)).mkString("[", ", ", "]")
  }

  def reset = synchronized {
    queues.clear
  }

  private def empty(key: String): Boolean = {
    queues.get(key) match {
      case None => true
      case Some(queue) => {
        queue.isEmpty
      }
    }
  }

  /**
   * For unit tests: Waits (until a timeout) for a queue to become empty, and returns
   * true on success, or false on timeout.
   */
  def waitForEmpty(key: String, timeoutMsec: Int): Boolean = synchronized {
    val expire = System.currentTimeMillis + timeoutMsec
    var remaining = timeoutMsec
    while (remaining > 0 && !empty(key)) {
      wait(remaining)
      remaining = (expire - System.currentTimeMillis).toInt
    }
    empty(key)
  }
}
