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


object ModuloNodeLocatorSpec extends Specification {

  def newTestLocator = {
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
    val pool = new ServerPool
    val connections = for (s <- servers) yield ServerPool.makeConnection(s, pool)
    pool.servers = connections.toArray
    val locator = new ModuloNodeLocator
    locator.setPool(pool)
    locator
  }

  def newTestLocatorWithWeights = {
    val servers = List(
      "10.0.1.1:11211 6",
      "10.0.1.2:11211 3",
      "10.0.1.3:11211 2",
      "10.0.1.4:11211 5",
      "10.0.1.5:11211 1",
      "10.0.1.6:11211 8",
      "10.0.1.7:11211 4",
      "10.0.1.8:11211 1"
    )
    val pool = new ServerPool
    val connections = for (s <- servers) yield ServerPool.makeConnection(s, pool)
    pool.servers = connections.toArray
    val locator = new ModuloNodeLocator
    locator.setPool(pool)
    locator
  }


  "ModuloNodeLocator" should {
    "find nodes" in {
      val expected = List(
        List("apple", "10.0.1.1"),
        List("beanie baby", "10.0.1.7"),
        List("california", "10.0.1.7"),
        List("dead dog", "10.0.1.2"),
        List("entrenched", "10.0.1.8"),
        List("FFS", "10.0.1.6"),
        List("GIGO", "10.0.1.5"),
        List("hello sailor", "10.0.1.2"),
        List("inner universe", "10.0.1.7"),
        List("jump!", "10.0.1.5"),
        List("kangaroo meat", "10.0.1.5")
      )

      val locator = newTestLocator
      var count = 0
      for (testcase <- expected) {
        val connection = locator.findNode(testcase(0).getBytes("utf-8"))
        if (connection.hostname != testcase(1)) {
          println("testcase line " + (count + 1))
        }
        connection.hostname mustEqual testcase(1)
        count += 1
      }
    }

    "find weighted nodes" in {
      val expected = List(
        List("apple", "10.0.1.2"),
        List("beanie baby", "10.0.1.6"),
        List("california", "10.0.1.2"),
        List("dead dog", "10.0.1.1"),
        List("entrenched", "10.0.1.6"),
        List("FFS", "10.0.1.4"),
        List("GIGO", "10.0.1.4"),
        List("hello sailor", "10.0.1.4"),
        List("inner universe", "10.0.1.6"),
        List("jump!", "10.0.1.5"),
        List("kangaroo meat", "10.0.1.6")
      )

      val locator = newTestLocatorWithWeights
      var count = 0
      for (testcase <- expected) {
        val connection = locator.findNode(testcase(0).getBytes("utf-8"))
        if (connection.hostname != testcase(1)) {
          println("testcase line " + (count + 1))
        }
        connection.hostname mustEqual testcase(1)
        count += 1
      }
    }
  }
}
