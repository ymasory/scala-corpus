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

import _root_.java.util.concurrent.CountDownLatch
import _root_.net.lag.configgy.Config
import _root_.org.specs._


object KestrelSpec extends Specification {

  def duration(f: => Unit): Int = {
    val startTime = System.currentTimeMillis
    f
    (System.currentTimeMillis - startTime).toInt
  }


  "KestrelClient" should {
    var store: MemoryStore = null
    var client: KestrelClient = null

    doBefore {
      store = new MemoryStore
      client = new KestrelClient(store)
    }


    "configure a mock" in {
      val client1 = new KestrelClient(Config.fromMap(Map("mock" -> "true")))
      val client2 = new KestrelClient(Config.fromMap(Map("servers" -> "localhost")))
      client1.messageStore.getClass.toString.contains("Memory") mustBe true
      client2.messageStore.getClass.toString.contains("Memory") mustBe false
    }

    "set" in  {
      client.put("work", "apple")
      client.put("work", "rabbit")
      client.put("leisure", "beartrap")
      store.toString mustEqual "[leisure=List(beartrap), work=List(apple, rabbit)]"
    }
    
    "set with expiry" in {
      client.put("work", "apple", 5000)
      client.put("work", "rabbit") // No expiry.
      client.put("leisure", "beartrap", 10000)
      store.toString mustEqual "[leisure=List(beartrap), work=List(apple, rabbit)]"
    }

    "get" in {
      client.put("work", "apple")
      client.put("work", "rabbit")
      client.get("work") mustEqual Some("apple")
      client.get("work") mustEqual Some("rabbit")
    }

    "get two queues" in {
      client.put("work1", "apple")
      client.put("work2", "rabbit")
      client.get("work1") mustEqual Some("apple")
      client.get("work2") mustEqual Some("rabbit")
    }

    "get with timeout" in {
      client.put("work", "apple")
      client.put("work", "rabbit")
      client.get("work", 25) mustEqual Some("apple")

      // should fail, but take at least 50ms
      duration { client.get("leisure", 50) mustEqual None } must be_>=(50)

      client.get("work", 50) mustEqual Some("rabbit")

      duration { client.get("work", 50) mustEqual None } must be_>=(50)
    }

    "get honors short timeout" in {
      duration { client.get("leisure", 1) mustEqual None } must be_<(50)
    }

    "poll" in {
      client.poll("work") mustEqual None
      client.put("work", "pear")
      client.poll("leisure") mustEqual None
      client.poll("work") mustEqual Some("pear")
      client.poll("work") mustEqual None
    }

    "stop when asked but no sooner" in {
      // Use flag to get status back from thread, as Specs seems to
      // get confused otherwise.
      @volatile var flag = 1
      val trigger = new CountDownLatch(1)
      val thread = new Thread() {
        override def run() = {
          trigger.countDown
          client.get("empty")
          if (flag == 2) {
            flag = 3
          } else {
            flag = -1
          }
        }
      }
      thread.start
      trigger.await
      flag must be(1)
      Thread.sleep(99)
      flag must be(1)
      flag = 2
      client.shutdown
      duration { thread.join(1000) } must be_<(250)
      flag must be(3)
    }

    "stop when interrupted but no sooner" in {
      // Use flag to get status back from thread, as Specs seems to
      // get confused otherwise.
      @volatile var flag = 1
      val trigger = new CountDownLatch(1)
      val thread = new Thread() {
        override def run() = {
          trigger.countDown
          try {
            client.get("empty")
          } catch {
            case e: InterruptedException =>
              if (flag == 2) {
                flag = 3
              } else {
                flag = -1
              }
            case e: Exception =>
              flag = -2
          }
        }
      }
      thread.start
      trigger.await
      flag must be(1)
      Thread.sleep(99)
      flag must be(1)
      flag = 2
      thread.interrupt
      duration { thread.join(1000) } must be_<(250)
      flag must be(3)
    }

    "signal an empty queue for other unit tests" in {
      client.put("fake", "sasquatch")
      new Thread() {
        override def run() = {
          client.get("fake")
        }
      }.start
      duration { store.waitForEmpty("fake", 1000) } must be_<(1000)
    }

    "signal an unused empty queue immediately for other unit tests" in {
      new Thread() {
        override def run() = {
          client.get("fake")
        }
      }.start
      duration { store.waitForEmpty("fake", 1000) } must be_<(1000)
    }

    "iterate a queue" in {
      val trigger = new CountDownLatch(1)
      new Thread() {
        override def run() = {
          client.put("things", "500")
          client.put("things", "23")
          trigger.await
          client.pause
        }
      }.start
      var sum = 0
      for (count <- client.queueIterator("things") { item => item.toInt }) {
        sum += count
        if (count == 23) {
          trigger.countDown
        }
      }
      sum mustEqual 523
    }
  }
}
