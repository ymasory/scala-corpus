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

import net.lag.configgy.Config
import org.specs._


object ServerPoolSpec extends Specification {
  "ServerPool" should {
    "parse a server description" in {
      val m1 = ServerPool.makeConnection("10.0.1.1:11211 600", null)
      m1.hostname mustEqual "10.0.1.1"
      m1.port mustEqual 11211
      m1.weight mustEqual 600
      val m2 = ServerPool.makeConnection("10.0.1.2:11511 300", null)
      m2.hostname mustEqual "10.0.1.2"
      m2.port mustEqual 11511
      m2.weight mustEqual 300
    }

    "read a config file" in {
      getClass.getClassLoader.getResourceAsStream("ketama_results").read()
      val config = Config.fromResource("test1.conf", getClass.getClassLoader)
      val pool = ServerPool.fromConfig(config.configMap("memcache"))
      pool.servers.size mustEqual 77
      pool.servers(0).toString must include("daemon001:11211 weight=1")
      pool.servers(1).toString must include("daemon002:11211 weight=1")
      pool.servers(23).toString must include("cluster007:11211 weight=1")
      pool.servers(76).toString must include("cluster068:11211 weight=2")
      pool.readTimeout mustEqual 3000
      pool.retryDelay mustEqual 42000
      pool.connectTimeout mustEqual 266
      pool.connector.getConnectTimeoutMillis() mustEqual 266
      pool.shouldRebalance mustEqual false
    }
  }
}
