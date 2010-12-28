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

import _root_.java.util.concurrent.atomic.AtomicInteger
import _root_.java.util.concurrent.{CountDownLatch, TimeUnit}
import _root_.net.lag.configgy.{Configgy, Config}
import _root_.net.lag.extensions._
import _root_.net.lag.logging.{Level, Logger}
import _root_.net.lag.smile._
import _root_.com.twitter.actors._
import _root_.scala.collection.mutable.ListBuffer

case class ReadRow(timestampMs:Long, value: String)
case class WrittenRow(timestampMs: Long, value: String)
case class InspectorFinished()
case class ReaderFinished()
case class ExtraneousMessage(id: Int)

class KestrelTest extends StressTest {
  val log = Logger.get
  val pollInitialMs = 10L
  val pollMaxMs = 250L
  val pollMultiplicand = 1.5
  val maxOutstanding = 1000  // Feather back writer throttle if it should get too far ahead
  val outstanding = new AtomicInteger(0)
  val errorBackoffMs = 500
  val config = new Config()

  class Writer(val queue: String, count: Int, size: Int, sizeVariance: Int, pauseMsOnTens: Int,
               startPauseMs: Int, inspector: Inspector) extends Actor
  {
    private val cache = MemcacheClient.create(config)
    private var id = 0
    private var outstanding = inspector.outstanding
    private var valueBase = generateValue(size + sizeVariance)
    private var timeouts = 0
    private var offline = 0
    val name = "Writer " + queue

    def act {
      Thread.sleep(startPauseMs)
      log.trace("%s starting", name)
      while (id < count) {
        val valueLen = size + nextInt(sizeVariance)
        val value = id.toString + " " + valueBase.substring(0, valueLen)
        val row = new WrittenRow(System.currentTimeMillis, value)

        inspector ! row

        var done = false
        while (!done) {
          try {
            cache.set(queue, value)
            done = true
          } catch {
            case e: MemcacheServerTimeout => {
              log.warning("%s got a timeout", name)
              timeouts += 1
              Thread.sleep(errorBackoffMs)
            }
            case e: MemcacheServerOffline => {
              log.warning("%s got an offline", name)
              offline += 1
              Thread.sleep(errorBackoffMs)
            }
            case e: Exception => {
              log.fatal("%s caught exception %s", name, e.toString)
              throw e
            }
          }
        }
        log.trace("%s wrote %s", name, row)

        id += 1
        if (id % 10 == 0) Thread.sleep(pauseMsOnTens + nextInt(pauseMsOnTens))
        var sleepMs = 400
        while (outstanding.get() > maxOutstanding) {
          log.debug("%s throttling %d", name, sleepMs)
          Thread.sleep(sleepMs)
          sleepMs = 3200 min sleepMs * 2
        }
        receiveWithin(0) {
          case TIMEOUT => null
          case e: ExtraneousMessage => {
            log.trace("%s response to extraneous message %d", name, e.id)
            reply(e)
          }
        }
      }
      if (timeouts > 0 || offline > 0) {
        log.warning("%s timeouts=%d offline=%d", name, timeouts, offline)
      }
      log.debug("%s exiting", name)
      cache.shutdown()
      exit()
    }
  }

  class Reader(val queue: String, pauseMsOnTens: Int, startPauseMs: Int,
               inspector: Inspector) extends Actor
  {
    private val cache = MemcacheClient.create(config)
    private var rowsRead = 0
    private var timeouts = 0
    private var offline = 0
    val name = "Reader " + queue

    var sleepMs = pollInitialMs

    def act {
      Thread.sleep(startPauseMs)
      log.trace("%s starting", name)
      while (true) {
        try {
          cache.get(queue) match {
            case None => {
              Thread.sleep(sleepMs)
              sleepMs = pollMaxMs min (sleepMs * pollMultiplicand).toLong
            }
            case Some(value) => {
              val row = new ReadRow(System.currentTimeMillis, value)
              log.trace("%s read %s", name, row)
              inspector ! row
              sleepMs = pollInitialMs
            }
          }
        } catch {
          case e: MemcacheServerTimeout => {
            log.warning("%s got a timeout", name)
            timeouts += 1
            Thread.sleep(errorBackoffMs)
          }
          case e: MemcacheServerOffline => {
            log.warning("%s got an offline", name)
            offline += 1
            Thread.sleep(errorBackoffMs)
          }
          case e: Exception => {
            log.fatal("%s caught exception %s", name, e)
            throw e
          }
        }
        receiveWithin(0) {
          case TIMEOUT => null
          case InspectorFinished => {
            if (timeouts > 0 || offline > 0) {
              log.warning("%s timeouts=%d offline=%d", name, timeouts, offline)
            }
            log.debug("%s exiting", name)
            reply(ReaderFinished)
            cache.shutdown()
            exit()
          }
          case e: ExtraneousMessage => {
            log.trace("%s response to extraneous message %d", name, e.id)
            reply(e)
          }
          case unknown =>
            throw new Exception(name + " received unknown " + unknown.toString)
        }
        if (rowsRead % 10 == 0) Thread.sleep(pauseMsOnTens + nextInt(pauseMsOnTens))
      }
    }
  }

  class Inspector(val queue: String, count: Int) extends Actor {
    private var minLatencyMs = Math.MAX_LONG
    private var maxLatencyMs = Math.MIN_LONG
    private var totalLatencyMs = 0L
    private var rowsChecked = 0
    private var bytesRead = 0L
    private val writtenRows = new ListBuffer[WrittenRow]()
    private val readRows = new ListBuffer[ReadRow]()
    private var reader: Reader = _
    private var writer: Writer = _
    val finished = new CountDownLatch(1)
    val outstanding = new AtomicInteger()
    val name = "Inspector " + queue

    def set(reader: Reader, writer: Writer) {
      this.reader = reader
      this.writer = writer
    }

    def act {
      while (true) {
        val wSize = writtenRows.size
        val rSize = readRows.size
        outstanding.set(wSize - rSize)
        if (wSize > 0 && rSize > 0) {
          val readRow = readRows.remove(0)
          val writtenRow = writtenRows.remove(0)
          log.trace("%s found rows written=%s read=%s", name, writtenRow, readRow)

          val latencyMs = readRow.timestampMs - writtenRow.timestampMs
          minLatencyMs = minLatencyMs min latencyMs
          maxLatencyMs = maxLatencyMs max latencyMs
          totalLatencyMs += latencyMs

          if (writtenRow.value != readRow.value) {
            log.fatal("%s expected=%s received=%s", name, writtenRow, readRow)
            assert(false)
          }
          rowsChecked += 1
          bytesRead += readRow.value.length
          if (rowsChecked % 250 == 0) {
            log.debug("%s writeRows.size=%d readRows.size=%d", name, wSize, rSize)
          }

          if (rowsChecked >= count) {
            if (writtenRows.size > 0) {
              log.fatal("%s expect written empty, size=%d", name, writtenRows.size)
              assert(false)
            }
            if (readRows.size > 0) {
              log.fatal("%s expect readempty, size=", name, writtenRows.size)
              assert(false)
            }

            log.info("%s PASSED: latency min/avg/max = %d / %d / %d bytes avg/total %d %d",
                     name, minLatencyMs, (totalLatencyMs / rowsChecked).toInt, maxLatencyMs,
                     (bytesRead / rowsChecked).toInt, bytesRead)

            // Wait for reader to return to ensure it cannot bork next test.
            reader !? InspectorFinished
            finished.countDown()
            log.debug("%s exiting", name)
            exit()
          }
        }
        receive {
          case row: WrittenRow => writtenRows += row
          case row: ReadRow => readRows += row
          case e: ExtraneousMessage => {
            // Loop through reader and writer
            reply(writer !? (reader !? e))
          }
          case unknown =>
            log.fatal("%s received unknown message %s", name, unknown.toString)
        }
      }
    }
  }

  def drain(queue: String) {
    log.debug("drain %s", queue)
    val cache = MemcacheClient.create(config)
    var done = false
    var rows = 0
    var timeout = 0
    var offline = 0

    while (!done) {
      try {
        cache.get(queue) match {
          case None => done = true
          case Some(value) => rows += 1
        }
      } catch {
        case e: MemcacheServerOffline => {
          log.debug("%s drain got an offline", queue)
          offline += 1
          Thread.sleep(errorBackoffMs)
        }
        case e: MemcacheServerTimeout => {
          log.debug("%s drain got a timeout", queue)
          timeout += 1
          Thread.sleep(errorBackoffMs)
        }
      }
    }
    log.debug("%s drained %d rows with %d timeouts %d offline", queue, rows, timeout, offline)
    cache.shutdown
  }

  def nextInt(range: Int) = if (range == 0) 0 else rnd.nextInt(range)

  class Params() {
    var queues: List[String] = _
    var count: Int = _
    var size: Int = _
    var sizeVariance: Int = _
    var writePauseOnTensMs: Int = _
    var readPauseOnTensMs: Int = _
    var writeStartPauseMs: Int = _
    var readStartPauseMs: Int = _
  }

  def testSetup(p: Params): List[Inspector] = {
    log.info("TEST host=%s queues=%s", hosts(0), p.queues)
    log.info("count=%d size=%d sizeVariance=%d", p.count, p.size, p.sizeVariance)
    log.info("writePauseOnTensMs=%d readPauseOnTensMs=%d",
             p.writePauseOnTensMs, p.readPauseOnTensMs)
    log.info("writeStartPauseMs=%d readStartPauseMs=%d", p.writeStartPauseMs, p.readStartPauseMs)

    p.queues.foreach(queue => drain(queue))

    var inspectors = new ListBuffer[Inspector]

    for (queue <- p.queues) {
      var inspector = new Inspector(queue, p.count)
      inspectors += inspector
      var reader = new Reader(queue, p.readPauseOnTensMs, p.readStartPauseMs, inspector)
      var writer = new Writer(
        queue, p.count, p.size, p.sizeVariance, p.writePauseOnTensMs, p.writeStartPauseMs,
        inspector)
      inspector.set(reader, writer)

      inspector.start
      reader.start
      writer.start
    }

    inspectors.toList
  }

  def testAwait(inspectors: List[Inspector]) {
    for (inspector <- inspectors) {
      log.trace("await inspector %s", inspector.name)
      while (!inspector.finished.await(10, TimeUnit.SECONDS)) {
        log.debug("await inspector %s", inspector.name)
      }
    }
    log.trace("TEST finished")
  }

  def test(p: Params) {
    testAwait(testSetup(p))
  }

  // Pester Reader and Writer Actors with extraneous messages,
  // ensuring that Smile doesn't attempt to eat them.
  // Note: Will hang if Writer finishes while still pestering actors...
  def testPester(count: Int) {
    val p = new Params()
    p.queues = queueList("smile-testpester", 1)
    p.count = count
    p.size = 256
    p.sizeVariance = 256
    // Make a slightly slower running test, to allow more waiting in Smile
    p.writePauseOnTensMs = 1
    p.readPauseOnTensMs = 3
    p.writeStartPauseMs = 0
    p.readStartPauseMs = 0

    val inspectors = testSetup(p)
    assert(inspectors.size == 1)
    val inspector = inspectors(0)
    // Writer actor will stop responding after its finished, so only
    // pester for a short while.
    for (idx <- 0 to count / 4) {
      val sent = new ExtraneousMessage(idx)
      log.trace("sent extraneous %d", idx)
      inspector !? sent match {
        case recv: ExtraneousMessage => {
          log.trace("recv extraneous %d", recv.id)
          assert(recv.id == sent.id)
        }
        case unknown => assert(false)
      }
    }
    testAwait(inspectors)
  }

  def queueList(base: String, count: Int): List[String] = {
    val rv = new ListBuffer[String]
    for (idx <- 1 to count) {
      rv += base + ":" + idx
    }
    rv.toList
  }

  def suite(queues: Int, count: Int, size: Int, variance: Int,
            startPauseMs: Int, rowPauseMs: Int)
  {
    // Simultaneous producer consumer (with small counts, devolves to a late consumer)
    val p = new Params()
    p.count = count
    p.size = size
    p.sizeVariance = variance

    // Simultaneous
    p.queues = queueList("smile-simultaneous", queues)
    p.writePauseOnTensMs = 0
    p.readPauseOnTensMs = 0
    p.writeStartPauseMs = 0
    p.readStartPauseMs = 0
    test(p)

    // Late producer
    p.queues = queueList("smile-lateproducer", queues)
    p.writeStartPauseMs = startPauseMs
    p.readStartPauseMs = 0
    test(p)

    // Late consumer
    p.queues = queueList("smile-lateconsumer", queues)
    p.writeStartPauseMs = 0
    p.readStartPauseMs = startPauseMs
    test(p)

    // Slow producer
    p.queues = queueList("smile-slowproducer", queues)
    p.writePauseOnTensMs = rowPauseMs
    p.readPauseOnTensMs = 0
    p.writeStartPauseMs = 0
    p.readStartPauseMs = 0
    test(p)

    // Slow consumer
    p.queues = queueList("smile-slowconsumer", queues)
    p.writePauseOnTensMs = 0
    p.readPauseOnTensMs = rowPauseMs
    test(p)
  }

  def go() {
    // The default retry_delay really drags a test out in the face of a network issue.
    config.setInt("retry_delay", 5)
    // The default read_timeout is a bit too tetchy when faced with even moderate network latency
    // For example, even over Wifi, the 2 second timeout is occasionally hit.
    config.setInt("read_timeout", 8)
    log.debug("Restting read_timeout to %d", config.getInt("read_timeout").get)
    config.setString("servers", hosts(0))
    config.setString("distribution", "default")
    config.setString("hash", "crc32-itu")

    //
    // Unit-like tests
    //
    log.debug("unit-like tests")
    suite(1, 3, 10, 0, 250, 250)

    //
    // Test client actor compatability.
    //
    log.debug("pester tests")
    testPester(512)

    //
    // Single-Queue tests
    //
    log.debug("single-queue tests")
    suite(1, 20000, 256, 8000, 250, 5)

    //
    // 3-Queue tests
    //
    log.debug("3-queue 1 tests")
    suite(3, 10000, 256, 8000, 250, 10)
    log.debug("3-queue 2 tests")
    suite(3, 10000, 2, 4, 250, 5)

    // 12-Queue tests
    log.debug("12-queue 1 tests")
    suite(12, 5000, 2, 4, 1000, 10)
    log.debug("12-queue list tests")
    List(2, 15, 25, 50, 75).foreach(pause => suite(12, 1000, 2, 4, pause * 10, pause))

    log.info("TESTS PASS")
  }
}

object Kestrel {
  def main(args: Array[String]): Unit = {
    val hostPort = System.getProperty("hostport")
    if (hostPort == null) {
      println("hostport property unset. -Dhostport=\"localhost:22133\"")
    } else {
      Configgy.configure("test.conf")
      val k = new KestrelTest()
      k.setHosts(Array(hostPort))
      k.go()
    }
  }
}
