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

import _root_.net.lag.configgy.Configgy
import _root_.org.specs._
import _root_.scala.collection.mutable
import _root_.java.io.{BufferedReader, InputStreamReader}


object SequentialNodeLocatorSpec extends Specification {

  val servers = List(
    "10.0.1.1:11211",
    "10.0.1.2:11211",
    "10.0.1.3:11211",
    "10.0.1.4:11211",
    "10.0.1.5:11211",
    "10.0.1.6:11211",
    "10.0.1.7:11211",
    "10.0.1.8:11211"
  )

  val serversWeighted = List(
    "10.0.1.1:11211 6",
    "10.0.1.2:11211 3",
    "10.0.1.3:11211 2",
    "10.0.1.4:11211 5",
    "10.0.1.5:11211 1",
    "10.0.1.6:11211 8",
    "10.0.1.7:11211 4",
    "10.0.1.8:11211 1"
  )

  // Need to go around at least twice to know that this will work
  val rounds = 5

  def newTestLocator(addresses: List[String]) = {
    val pool = new ServerPool
    val connections = for (s <- addresses) yield ServerPool.makeConnection(s, pool)
    pool.servers = connections.toArray
    val locator = new SequentialNodeLocator
    locator.setPool(pool)
    locator
  }

  def runner(addresses: List[String]) = {
    val locator = newTestLocator(addresses)
    val found = new mutable.HashMap[String, Int]
    val weight = new mutable.HashMap[String, Int]
    var entries = 0
    for (address <- addresses) {
      val split = address.split(" ")
      val weighting = if (split.length > 1) {
        Integer.parseInt(split(1))
      } else {
        1
      }
      weight += split(0) -> weighting
      entries += weighting
    }

    for (round <- 1 to rounds) {
      for (idx <- 1 to entries) {
        val node = locator.findNode("queueName".getBytes("utf-8"))
        val host = node.hostname + ":" + node.port
        val count = found.getOrElseUpdate(host, 0)
        found(host) = count + 1
      }
    }
    found.size must be_==(addresses.length)
    for (address <- addresses) {
      val server = address.split(" ")(0)
      found.contains(server) must beTrue
      found(server) must be_==(rounds * weight(server))
    }
  }

  "SequentialNodeLocator" should {
    "find unweighted nodes" in {
      runner(servers)
    }

    "find weighted nodes" in {
      runner(serversWeighted)
    }
  }
}
