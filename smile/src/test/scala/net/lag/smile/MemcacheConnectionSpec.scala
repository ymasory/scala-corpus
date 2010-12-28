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

import _root_.net.lag.naggati.Steps._
import _root_.java.nio.ByteOrder
import _root_.com.twitter.xrayspecs.Time
import _root_.com.twitter.xrayspecs.TimeConversions._
import _root_.org.apache.mina.core.buffer.IoBuffer
import _root_.org.apache.mina.core.session.{AbstractIoSession, DummySession, IoSession}
import _root_.org.apache.mina.filter.codec._
import _root_.org.specs._
import net.lag.smile.MemcacheConnection.{ConnectionFailed, ConnectionEjected}

object MemcacheConnectionSpec extends Specification {

  var pool: ServerPool = null
  var server: FakeMemcacheConnection = null
  var conn: MemcacheConnection = null


  def data(v: Option[MemcacheResponse.Value]) = {
    v match {
      case None => ""
      case Some(value) => new String(value.data, "utf-8")
    }
  }

  def data(map: Map[String, MemcacheResponse.Value]) = {
    Map.empty[String, String] ++ (for ((k, v) <- map) yield (k, new String(v.data, "utf-8")))
  }


  "MemcacheConnection" should {
    doBefore {
      pool = new ServerPool
    }

    doAfter {
      if (server ne null) {
        server.stop
      }
      if (conn != null) {
        conn.shutdown
      }
      conn = null
    }


    "connect to localhost" in {
      server = new FakeMemcacheConnection(Nil)
      server.start

      conn = new MemcacheConnection("localhost", server.port, 1)
      conn.pool = pool
      conn.connectionError mustBe None
      server.awaitConnection(500) mustBe true
    }

    "correctly indicate a failed connection" in {
      server = new FakeMemcacheConnection(Nil)
      server.start

      conn = new MemcacheConnection("localhost", server.port + 1, 1)
      conn.pool = pool
      conn.connectionError must beSome(ConnectionFailed)
      server.awaitConnection(500) mustBe false
    }

    "correctly indicate an ejected connection" in {
      server = new FakeMemcacheConnection(Nil)
      server.start

      conn = new MemcacheConnection("localhost", server.port + 1, 1)
      conn.pool = pool
      conn.delaying = Some(Time.now + pool.retryDelay.milliseconds)
      conn.connectionError must beSome(ConnectionEjected)
      server.awaitConnection(500) mustBe false
    }

    "attempt a reconnect if a server disconnects" in {
      server = new FakeMemcacheConnection(Disconnect :: Send("VALUE fail 0 2\r\nno\r\nEND\r\n".getBytes) :: Nil)
      server.start

      conn = new MemcacheConnection("localhost", server.port, 1)
      conn.pool = pool
      conn.connectionError mustBe None
      server.awaitConnection(500) mustBe true
      server.awaitDisconnected(500) mustBe true
      server.awaitConnection(50) mustBe false
      (
        try {
          data(conn.get("fail"))
        } catch {
          case _: net.lag.smile.MemcacheServerTimeout => "TIMEOUT"
        }
      ) mustEqual("no")

      conn.connectionError mustBe None
      server.awaitConnection(500) mustBe true
    }

    "eject a server when it vanishes, and try again after a delay" in {
      server = new FakeMemcacheConnection(Receive(10) :: Send("VALUE fail 0 2\r\nno\r\nEND\r\n".getBytes) ::
        KillListenSocket :: Disconnect :: Nil)
      server.start

      conn = new MemcacheConnection("localhost", server.port, 1)
      conn.pool = pool
      conn.connectionError mustBe None
      server.awaitConnection(500) mustBe true
      data(conn.get("fail")) mustEqual "no"

      server.awaitDisconnected(500) mustBe true
      server.awaitConnection(1) mustBe false

      conn.connectionError must beSome(ConnectionFailed)
      conn.delaying must beSome[Time]
      conn.session mustEqual None
      server.stop

      // now verify that the server comes back.
      Time.advance(pool.retryDelay.milliseconds + 1.millisecond)
      server = new FakeMemcacheConnection(Receive(10) :: Send("VALUE fail 0 3\r\nyes\r\nEND\r\n".getBytes) :: Nil, server.port)
      server.start
      conn.connectionError mustBe None
      server.awaitConnection(500) mustBe true
      data(conn.get("fail")) mustEqual "yes"
    }

    "not update the retry delay when a connection is retried while the connection is still ejected" in {
      pool.maxFailuresBeforeEjection = 1
      server = new FakeMemcacheConnection(Receive(10) :: Disconnect :: Receive(10) :: Disconnect :: Nil)
      server.start

      conn = new MemcacheConnection("localhost", server.port, 1)
      conn.pool = pool
      conn.connectionError mustBe None
      server.awaitConnection(500) mustBe true
      conn.get("fail") must throwA[MemcacheServerException]
      conn.isEjected must beTrue
      val delayTime = conn.delaying
      delayTime must beSome[Time]

      Time.advance(pool.retryDelay.milliseconds - 1.millisecond)      
      conn.get("fail") must throwA[MemcacheServerException]
      conn.delaying must beEqual(delayTime)
    }

    "eject a server after N consecutive failures" in {
      pool.maxFailuresBeforeEjection = 2
      server = new FakeMemcacheConnection(Receive(10) :: Disconnect :: Receive(10) :: Disconnect :: Nil)
      server.start

      conn = new MemcacheConnection("localhost", server.port, 1)
      conn.pool = pool
      conn.connectionError mustBe None
      server.awaitConnection(500) mustBe true
      conn.get("fail") must throwA[MemcacheServerException]
      conn.isEjected must beFalse
      conn.get("fail") must throwA[MemcacheServerException]
      conn.isEjected must beTrue
    }

    "get" in {
      "a single value" in {
        server = new FakeMemcacheConnection(Receive(9) ::
          Send("VALUE cat 0 5\r\nhello\r\nEND\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        data(conn.get("cat")) mustEqual "hello"
        server.fromClient mustEqual List("get cat\r\n")
      }

      "an empty value" in {
        server = new FakeMemcacheConnection(Receive(9) :: Send("END\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.get("cat") mustEqual None
        server.fromClient mustEqual List("get cat\r\n")
      }

      "multiple values" in {
        server = new FakeMemcacheConnection(Receive(13) ::
          Send("VALUE cat 0 5\r\nhello\r\nVALUE dog 0 7\r\ngoodbye\r\nEND\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        data(conn.get(Array("cat", "dog"))) mustEqual Map("cat" -> "hello", "dog" -> "goodbye")
        server.fromClient mustEqual List("get cat dog\r\n")
      }

      "two on the same connection" in {
        server = new FakeMemcacheConnection(Receive(9) ::
          Send("VALUE cat 0 5\r\nhello\r\nEND\r\n".getBytes) ::
          Receive(9) ::
          Send("VALUE dog 0 7\r\ngoodbye\r\nEND\r\n".getBytes) ::
          Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        data(conn.get("cat")) mustEqual "hello"
        data(conn.get("dog")) mustEqual "goodbye"
        server.fromClient mustEqual List("get cat\r\n", "get dog\r\n")
      }

      "timeout" in {
        server = new FakeMemcacheConnection(Receive(9) :: Sleep(200) :: Send("END\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.pool.readTimeout = 100
        data(conn.get("cat")) must throwA(new MemcacheServerTimeout)
      }

      "throw an exception for a bad server" in {
        server = new FakeMemcacheConnection(Receive(9) ::
          Send("CLIENT_ERROR i feel ill\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        data(conn.get("cat")) must throwA[MemcacheServerException]
      }
    }

    "set" in {
      "a single value" in {
        server = new FakeMemcacheConnection(Receive(24) :: Send("STORED\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.set("cat", "hello".getBytes, 0, 500)
        server.fromClient mustEqual List("set cat 0 500 5\r\nhello\r\n")
      }

      "with server error" in {
        server = new FakeMemcacheConnection(Receive(24) :: Send("ERROR\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.set("cat", "hello".getBytes, 0, 500) must throwA[MemcacheServerException]
        server.fromClient mustEqual List("set cat 0 500 5\r\nhello\r\n")
      }
    }

    "delete" in {
      "a single value" in {
        server = new FakeMemcacheConnection(Receive(12) :: Send("DELETED\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.delete("cat") mustEqual true
        server.fromClient mustEqual List("delete cat\r\n")
      }

      "not found" in {
        server = new FakeMemcacheConnection(Receive(12) :: Send("NOT_FOUND\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.delete("cat") mustEqual false
        server.fromClient mustEqual List("delete cat\r\n")
      }

      "with server error" in {
        server = new FakeMemcacheConnection(Receive(12) :: Send("ERROR\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.delete("cat") must throwA[MemcacheServerException]
        server.fromClient mustEqual List("delete cat\r\n")
      }
    }

    // impl is identical to "set"
    "add" in {
      "a single value" in {
        server = new FakeMemcacheConnection(Receive(24) :: Send("STORED\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.add("cat", "hello".getBytes, 0, 500) mustBe true
        server.fromClient mustEqual List("add cat 0 500 5\r\nhello\r\n")
      }

      "which returns NOT_STORED" in {
        server = new FakeMemcacheConnection(Receive(24) :: Send("NOT_STORED\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.add("cat", "hello".getBytes, 0, 500) mustBe false
        server.fromClient mustEqual List("add cat 0 500 5\r\nhello\r\n")
      }
    }

    // impl is identical to "set"
    "replace" in {
      "a single value" in {
        server = new FakeMemcacheConnection(Receive(28) :: Send("STORED\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.replace("cat", "hello".getBytes, 0, 500) mustBe true
        server.fromClient mustEqual List("replace cat 0 500 5\r\nhello\r\n")
      }

      "which returns NOT_STORED" in {
        server = new FakeMemcacheConnection(Receive(28) :: Send("NOT_STORED\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.replace("cat", "hello".getBytes, 0, 500) mustBe false
        server.fromClient mustEqual List("replace cat 0 500 5\r\nhello\r\n")
      }
    }

    // impl is identical to "set"
    "append" in {
      "a single value" in {
        server = new FakeMemcacheConnection(Receive(27) :: Send("STORED\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.append("cat", "hello".getBytes, 0, 500)
        server.fromClient mustEqual List("append cat 0 500 5\r\nhello\r\n")
      }
    }

    // impl is identical to "set"
    "prepend" in {
      "a single value" in {
        server = new FakeMemcacheConnection(Receive(28) :: Send("STORED\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.prepend("cat", "hello".getBytes, 0, 500)
        server.fromClient mustEqual List("prepend cat 0 500 5\r\nhello\r\n")
      }
    }

    "incr" in {
      "a single value" in {
        server = new FakeMemcacheConnection(Receive(12) :: Send("1\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.incr("cat", 1) mustEqual Some(1)
        server.fromClient mustEqual List("incr cat 1\r\n")
      }

      "not found" in {
        server = new FakeMemcacheConnection(Receive(12) :: Send("NOT_FOUND\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.incr("cat", 2) mustEqual None
        server.fromClient mustEqual List("incr cat 2\r\n")
      }

      "with server error" in {
        server = new FakeMemcacheConnection(Receive(12) :: Send("ERROR\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.incr("cat", 3) must throwA[MemcacheServerException]
        server.fromClient mustEqual List("incr cat 3\r\n")
      }
    }

    "decr" in {
      "a single value" in {
        server = new FakeMemcacheConnection(Receive(12) :: Send("1\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.decr("cat", 1) mustEqual Some(1)
        server.fromClient mustEqual List("decr cat 1\r\n")
      }

      "not found" in {
        server = new FakeMemcacheConnection(Receive(12) :: Send("NOT_FOUND\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.decr("cat", 2) mustEqual None
        server.fromClient mustEqual List("decr cat 2\r\n")
      }

      "with server error" in {
        server = new FakeMemcacheConnection(Receive(12) :: Send("ERROR\r\n".getBytes) :: Nil)
        server.start

        conn = new MemcacheConnection("localhost", server.port, 1)
        conn.pool = pool
        conn.decr("cat", 3) must throwA[MemcacheServerException]
        server.fromClient mustEqual List("decr cat 3\r\n")
      }
    }
  }
}
