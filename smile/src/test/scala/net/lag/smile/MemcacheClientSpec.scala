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

import _root_.java.util.concurrent.CountDownLatch
import _root_.scala.collection.mutable
import _root_.com.twitter.xrayspecs.Time
import _root_.com.twitter.xrayspecs.TimeConversions._
import _root_.org.specs.Specification
import _root_.org.specs.mock.{ClassMocker, JMocker}


class MemcacheClientSpec extends Specification with JMocker {
  var pool: ServerPool = null
  val servers = new mutable.ListBuffer[FakeMemcacheConnection]
  var client: MemcacheClient[String] = null
  var locator: NodeLocator = null
  val connections = new mutable.ListBuffer[MemcacheConnection]

  def makeServers(seed: List[List[Task]]) = {
    connections.clear()
    pool = new ServerPool
    for (tasks <- seed) {
      val server = new FakeMemcacheConnection(tasks)
      server.start
      servers += server
      val connection = new MemcacheConnection("localhost", server.port, 1)
      connection.pool = pool
      connections += connection
    }
    pool.servers = connections.toArray
    client = new MemcacheClient(locator, MemcacheCodec.UTF8)
    expect { one(locator).setPool(any) }
    client.setPool(pool)
  }

  "MemcacheClient" should {
    doBefore {
      locator = mock[NodeLocator]
    }

    doAfter {
      for (s <- servers) {
        s.stop
      }
      servers.clear()
      client.shutdown()
      connections.clear()
    }


    "get keys from 3 different servers" in {
      makeServers(List(
        Receive(7) :: Send("VALUE a 0 5\r\napple\r\nEND\r\n".getBytes) :: Nil,
        Receive(7) :: Send("VALUE b 0 5\r\nbeach\r\nEND\r\n".getBytes) :: Nil,
        Receive(7) :: Send("VALUE c 0 5\r\nconch\r\nEND\r\n".getBytes) :: Nil
      ))

      expect {
        one(locator).findNode("a".getBytes) willReturn connections(0)
        one(locator).findNode("b".getBytes) willReturn connections(1)
        one(locator).findNode("c".getBytes) willReturn connections(2)
      }

      client.get("a") mustEqual Some("apple")
      client.get("b") mustEqual Some("beach")
      client.get("c") mustEqual Some("conch")
      for (s <- servers) {
        s.awaitConnection(500) mustBe true
      }
      servers(0).fromClient mustEqual List("get a\r\n")
      servers(1).fromClient mustEqual List("get b\r\n")
      servers(2).fromClient mustEqual List("get c\r\n")
    }

    "get keys from 3 different servers simultaneously" in {
      makeServers(List(
        Receive(7) :: Sleep(100) :: Send("VALUE a 0 5\r\napple\r\nEND\r\n".getBytes) :: Nil,
        Receive(7) :: Sleep(100) :: Send("VALUE b 0 5\r\nbeach\r\nEND\r\n".getBytes) :: Nil,
        Receive(7) :: Sleep(100) :: Send("VALUE c 0 5\r\nconch\r\nEND\r\n".getBytes) :: Nil
      ))

      expect {
        one(locator).findNode("a".getBytes) willReturn connections(0)
        one(locator).findNode("b".getBytes) willReturn connections(1)
        one(locator).findNode("c".getBytes) willReturn connections(2)
      }

      val latch = new CountDownLatch(1)
      var val1: Option[String] = None
      var val2: Option[String] = None
      var val3: Option[String] = None
      val t1 = new Thread {
        override def run = {
          latch.await
          val1 = client.get("a")
        }
      }
      val t2 = new Thread {
        override def run = {
          latch.await
          val2 = client.get("b")
        }
      }
      val t3 = new Thread {
        override def run = {
          latch.await
          val3 = client.get("c")
        }
      }

      t1.start
      t2.start
      t3.start
      latch.countDown
      t1.join
      t2.join
      t3.join

      val1 mustEqual Some("apple")
      val2 mustEqual Some("beach")
      val3 mustEqual Some("conch")
      for (s <- servers) {
        s.awaitConnection(500) mustBe true
      }
      servers(0).fromClient mustEqual List("get a\r\n")
      servers(1).fromClient mustEqual List("get b\r\n")
      servers(2).fromClient mustEqual List("get c\r\n")
    }

    "multi-get keys from 3 different servers" in {
      makeServers(List(
        Receive(7) :: Send("VALUE a 0 5\r\napple\r\nEND\r\n".getBytes) :: Nil,
        Receive(7) :: Send("VALUE b 0 5\r\nbeach\r\nEND\r\n".getBytes) :: Nil,
        Receive(7) :: Send("VALUE c 0 5\r\nconch\r\nEND\r\n".getBytes) :: Nil
      ))

      expect {
        one(locator).findNode("a".getBytes) willReturn connections(0)
        one(locator).findNode("b".getBytes) willReturn connections(1)
        one(locator).findNode("c".getBytes) willReturn connections(2)
      }

      client.get(Array("a", "b", "c")) mustEqual Map("a" -> "apple", "b" -> "beach", "c" -> "conch")
      for (s <- servers) {
        s.awaitConnection(500) mustBe true
      }
      servers(0).fromClient mustEqual List("get a\r\n")
      servers(1).fromClient mustEqual List("get b\r\n")
      servers(2).fromClient mustEqual List("get c\r\n")
    }

    "multi-get keys from 1 server with namespacing" in {
      makeServers(List(
        Receive(17) :: Send(("VALUE a:a 0 5\r\napple\r\nVALUE a:b 0 5\r\n" +
          "beach\r\nVALUE a:c 0 5\r\nconch\r\nEND\r\n").getBytes) :: Nil
        ))
      client.namespace = Some("a:")

      expect {
        one(locator).findNode("a:a".getBytes) willReturn connections(0)
        one(locator).findNode("a:b".getBytes) willReturn connections(0)
        one(locator).findNode("a:c".getBytes) willReturn connections(0)
      }

      client.get(Array("a", "b", "c")) mustEqual Map("a" -> "apple", "b" -> "beach", "c" -> "conch")
      for (s <- servers) {
        s.awaitConnection(500) mustBe true
      }
      servers(0).fromClient mustEqual List("get a:a a:b a:c\r\n")
    }

    "reject keys that are too long" in {
      makeServers(List(Nil))
      client.get("x" * 300) must throwA(new KeyTooLongException)
    }

    "recover from a multi-get if one of the servers is down" in {
      makeServers(List(
        Receive(7) :: Send("VALUE a 0 5\r\napple\r\nEND\r\n".getBytes) :: Nil,
        KillListenSocket :: Nil
      ))

      expect {
        one(locator).findNode("a".getBytes) willReturn connections(0)
        one(locator).findNode("b".getBytes) willReturn connections(1)
      }

      client.get(Array("a", "b")) mustEqual Map("a" -> "apple")
    }

    "eject a bad server and retry it later" in {
      makeServers(List(
        Receive(7) :: Send("VALUE a 0 5\r\napple\r\nEND\r\n".getBytes) :: Receive(7) :: Send("END\r\n".getBytes) :: Nil,
        Receive(7) :: SkipAwaitConnection :: Disconnect :: Receive(7) :: SkipAwaitConnection :: Disconnect :: Receive(7) :: Send("VALUE a 0 1\r\nb\r\nEND\r\n".getBytes) :: Nil
      ))
      pool.maxFailuresBeforeEjection = 2

      expect {
        one(locator).findNode("a".getBytes) willReturn connections(0)
        one(locator).findNode("b".getBytes) willReturn connections(1)
      }

      client.get("a") mustEqual Some("apple")
      client.get("b") must throwA[MemcacheServerException]
      connections(1).isEjected must beFalse

      expect {
        one(locator).findNode("b".getBytes) willReturn connections(1)
        one(locator).setPool(any)
      }

      client.get("b") must throwA[MemcacheServerException]
      connections(1).isEjected must beTrue

      expect {
        one(locator).findNode("b".getBytes) willReturn connections(0)
      }
      client.get("b") mustEqual None

      Time.advance(60.seconds)
      connections(1).isEjected must beFalse

      expect {
        one(locator).setPool(any)
        one(locator).findNode("b".getBytes) willReturn connections(1)
      }

      client.get("b") mustEqual Some("b")
    }

    "should not set the pool on ejected servers if should_rebalance is set to false" in {
      makeServers(List(
        Receive(7) :: Send("VALUE a 0 5\r\napple\r\nEND\r\n".getBytes) :: Receive(7) :: Send("END\r\n".getBytes) :: Nil,
        Receive(7) :: SkipAwaitConnection :: Disconnect :: Receive(7) :: Send("VALUE a 0 1\r\nb\r\nEND\r\n".getBytes) :: Nil
      ))

      pool.shouldRebalance = false
      pool.maxFailuresBeforeEjection = 1

      expect {
        one(locator).findNode("b".getBytes) willReturn connections(1)
      }

      client.get("b") must throwA[MemcacheServerException]
      connections(1).isEjected must beTrue

      Time.advance(60.seconds)
      connections(1).isEjected must beFalse

      expect {
        one(locator).findNode("b".getBytes) willReturn connections(1)
      }

      client.get("b") mustEqual Some("b")
    }
  }
}
