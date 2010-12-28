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

import _root_.net.lag.configgy.Config
import _root_.org.specs._
import _root_.scala.collection.mutable
import _root_.java.io.{BufferedReader, InputStreamReader}


object KetamaNodeLocatorSpec extends Specification {

  def newTestLocator = {
    val servers = List(
      "10.0.1.1:11211 600",
      "10.0.1.2:11211 300",
      "10.0.1.3:11211 200",
      "10.0.1.4:11211 350",
      "10.0.1.5:11211 1000",
      "10.0.1.6:11211 800",
      "10.0.1.7:11211 950",
      "10.0.1.8:11211 100"
    )
    val pool = new ServerPool
    val connections = for (s <- servers) yield ServerPool.makeConnection(s, pool)
    pool.servers = connections.toArray
    val ketama = new KetamaNodeLocator
    ketama.setPool(pool)
    ketama
  }


  "KetamaNodeLocator" should {
    "be compatible with a standard benchmark" in {
      val stream = getClass.getClassLoader.getResourceAsStream("ketama_results")
      val reader = new BufferedReader(new InputStreamReader(stream))
      val expected = new mutable.ListBuffer[Array[String]]
      var line: String = null
      do {
        line = reader.readLine
        if (line != null) {
          val segments = line.split(" ")
          segments.length mustEqual 4
          expected += segments
        }
      } while (line != null)
      expected.size mustEqual 99

      val ketama = newTestLocator
      var count = 0
      for (testcase <- expected) {
        val connection = ketama.findNode(testcase(0).getBytes("utf-8"))
        if (connection.hostname != testcase(3)) {
          println("testcase line " + (count + 1))
        }
        connection.hostname mustEqual testcase(3)
        count += 1
      }
    }

    "be compatible with a test on a very large server list" in {
      getClass.getClassLoader.getResourceAsStream("ketama_results").read()
      val config = Config.fromResource("test1.conf", getClass.getClassLoader)
      val pool = ServerPool.fromConfig(config.configMap("memcache"))
      val ketama = new KetamaNodeLocator
      ketama.setPool(pool)
      ketama

      val expected = List(
        List("apple", "daemon003"),
        List("beanie baby", "cluster049"),
        List("california", "cluster027"),
        List("dead dog", "cluster062"),
        List("entrenched", "cluster001"),
        List("FFS", "cluster044"),
        List("GIGO", "cluster065"),
        List("hello sailor", "cluster049"),
        List("inner universe", "cluster015"),
        List("jump!", "daemon009"),
        List("kangaroo meat", "cluster001")
        )
      var count = 0
      for (testcase <- expected) {
        val connection = ketama.findNode(testcase(0).getBytes("utf-8"))
        if (connection.hostname != testcase(1)) {
          println("testcase line " + (count + 1))
        }
        connection.hostname mustEqual testcase(1)
        count += 1
      }
    }
  }
}
