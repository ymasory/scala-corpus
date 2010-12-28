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

package net.lag.smile.stress

import _root_.java.util.Random
import _root_.java.util.concurrent.CountDownLatch
import _root_.net.lag.configgy.Configgy
import _root_.net.lag.extensions._
import _root_.net.lag.logging.{Level, Logger}
import _root_.net.lag.smile.MemcacheClient


class ManyGetsTest extends StressTest {
  // get the same value N times in a row.
  def serialGets(count: Int, size: Int) = {
    val host = hosts(0)
    println("serialGets: count=%d size=%d host=%s".format(count, size, host))
    val cache = MemcacheClient.create(Array(host), "default", "crc32-itu")

    val key = "toasters"
    val value = generateValue(size)
    cache.set(key, value)

    report("toasters", count) {
      for (i <- 1 to count) {
        if (cache.get(key) != Some(value)) {
          throw new Exception("aiieeee!")
        }
      }
    }
    cache.shutdown
  }

  // get the same value N times in a row from each of M threads.
  def parallelGets(count: Int, threads: Int, size: Int) = {
    val host = hosts(0)
    println("parallel gets: count=%d threads=%d size=%d host=%s".format(count, threads, size, host))
    val cache = MemcacheClient.create(Array(host), "default", "crc32-itu")

    val key = "toasters"
    val value = generateValue(size)
    cache.set(key, value)

    val latch = new CountDownLatch(1)
    var threadList: List[Thread] = Nil

    for (i <- 1 to threads) {
      val t = new Thread {
        override def run() = {
          latch.await
          for (i <- 1 to count) {
            if (cache.get(key) != Some(value)) {
              throw new Exception("aiieeee!")
            }
          }
        }
      }
      t.start
      threadList = t :: threadList
    }

    report("toasters", count * threads) {
      latch.countDown
      for (t <- threadList) t.join
    }
    cache.shutdown
  }

  // Put a value, get a value, of slightly varying lengths
  def serialPutAndGet(count: Int, size: Int) = {
    println("serialPutAndGet: count=%d size=%d".format(count,size))
    val cache = MemcacheClient.create(Array(hosts(0)), "default", "crc32-itu")

    val key = "toasters"
    val numValues = 10
    val values: List[String] =
      (for (i <- 1 to numValues) yield generateValue(size + rnd.nextInt(10))).toList

    report("toasters", count) {
      for (i <- 1 to count / numValues) {
        for (value <- values) {
          cache.set(key, value)
          if (cache.get(key) != Some(value)) {
            throw new Exception("aiieeee!")
          }
        }
      }
    }
    cache.shutdown
  }

  // Generally put values, getting only occasionally to verify
  def serialPuts(count: Int, size: Int) = {
    println("serialPuts: count=%d size=%d".format(count,size))
    val cache = MemcacheClient.create(Array(hosts(0)), "default", "crc32-itu")

    val key = "toasters"
    val numValues = 10
    val values: List[String] = generateValues(size, 10, numValues)

    report("toasters", count) {
      for (i <- 1 to count / numValues) {
        for (value <- values) {
          cache.set(key, value)
          if (rnd.nextInt(100) % 1 == 0 && cache.get(key) != Some(value)) {
            throw new Exception("aiieeee!")
          }
        }
      }
    }
    cache.shutdown
  }

  // get one of K values, N times each from M threads, against 3 memcache servers.
  def parallelGetsFrom3(count: Int, threads: Int, keyCount: Int, size: Int) = {
    println("parallel gets: count=%d threads=%d size=%d from hosts=%s".format(count, threads, size, hosts.toString))
    val cache = MemcacheClient.create(hosts, "default", "crc32-itu")

    val keys: List[String] = (for (i <- 1 to keyCount) yield generateValue(size)).toList
    for (k <- keys) {
      cache.set(k, k)
    }
    for (s <- cache.servers) {
      if (! s.connected) {
        throw new Exception("not connected! to " + s)
      }
    }

    val latch = new CountDownLatch(1)
    var threadList: List[Thread] = Nil

    for (i <- 1 to threads) {
      val t = new Thread {
        override def run() = {
          val r = new Random
          latch.await
          for (i <- 1 to count) {
            val key = keys(r.nextInt(keys.length))
            val got = cache.get(key)
            if (got != Some(key)) {
              println("round " + i + ": got " + got + ", expected " + Some(key))
              throw new Exception("aiieeee!")
            }
          }
        }
      }
      t.start
      threadList = t :: threadList
    }

    report("toasters", count * threads) {
      latch.countDown
      for (t <- threadList) t.join
    }
    cache.shutdown
  }

  def test() {
    serialGets(1000, 10)
    serialPutAndGet(1000, 10)
    serialPutAndGet(1000, 10)
    serialPuts(1000, 10)
    parallelGets(1000, 10, 10)
    parallelGetsFrom3(1000, 10, 25, 10)

    serialGets(10000, 5000)
    serialGets(10000, 5001)
    serialPutAndGet(10000, 5002)
    parallelGets(10000, 100, 5003)
    serialPuts(10000, 5007)
    parallelGetsFrom3(4000, 100, 25, 6004)
    parallelGetsFrom3(4000, 100, 25, 15004)
  }
}


object ManyGets {
  def main(args: Array[String]): Unit = {
    if (args.size != 3) {
      println("Must specify 3 hosts")
    } else {
      val mg = new ManyGetsTest()
      mg.setHosts(args)
      mg.test()
    }
  }
}
