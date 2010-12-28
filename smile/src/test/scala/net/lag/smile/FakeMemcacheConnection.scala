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

import _root_.java.io.{InputStream, IOException, OutputStream}
import _root_.java.net.{ServerSocket, Socket}
import _root_.java.util.concurrent.{CountDownLatch, Semaphore, TimeUnit}
import _root_.scala.collection.mutable


abstract case class Task()
case class Receive(count: Int) extends Task
case class Send(data: Array[Byte]) extends Task
case class Sleep(ms: Int) extends Task
case object Disconnect extends Task
case object SkipAwaitConnection extends Task
case object KillListenSocket extends Task


class FakeMemcacheConnection(tasks: List[Task], var port: Int) extends Runnable {
  def this(tasks: List[Task]) = this(tasks, 0)

  val socket = new ServerSocket(port, 100)
  port = socket.getLocalPort

  val gotConnection = new Semaphore(0)
  val disconnected = new CountDownLatch(1)
  val thread = new Thread(this, "FakeMemcache")
  thread.setDaemon(true)

  val dataRead = new mutable.ListBuffer[Array[Byte]]

  var client: Socket = null
  var inStream: InputStream = null
  var outStream: OutputStream = null

  override def run() = try {
    while (true) {
      getClient

      for (t <- tasks) {
        t match {
          case Receive(count) =>
            var sofar = 0
            val buffer = new Array[Byte](count)
            while (sofar < count) {
              val n = inStream.read(buffer, sofar, count - sofar)
              if (n < 0) {
                throw new IOException("eof")
              }
              sofar += n
            }
            dataRead += buffer
          case Send(data) =>
            outStream.write(data)
          case Sleep(n) =>
            try {
              Thread.sleep(n)
            } catch {
              case x: InterruptedException =>
            }
          case SkipAwaitConnection =>
            awaitConnection(5000)
          case Disconnect =>
            while (gotConnection.availablePermits > 0) {
              Thread.sleep(50)
            }
            client.close
            disconnected.countDown
            getClient
          case KillListenSocket =>
            socket.close
        }
      }
    }
  } catch {
    case e: Exception =>
  }

  def start = {
    thread.start
  }

  def stop = {
    thread.interrupt
  }

  def getClient = {
    client = socket.accept
    inStream = client.getInputStream
    outStream = client.getOutputStream
    gotConnection.release
  }

  def awaitConnection(msec: Int) = {
    gotConnection.tryAcquire(msec, TimeUnit.MILLISECONDS)
  }

  def awaitDisconnected(msec: Int) = {
    disconnected.await(msec, TimeUnit.MILLISECONDS)
  }

  def fromClient(): List[String] = {
    dataRead map (new String(_)) toList
  }
}
