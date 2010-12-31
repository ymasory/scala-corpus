/*
 * Copyright 2010 LinkedIn
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package kafka.log

import java.io._
import java.nio._
import java.util.ArrayList
import junit.framework.TestCase
import junit.framework.Assert._
import kafka.message._
import kafka.utils._
import kafka.common._
import kafka.TestUtils

class LogTest extends TestCase {
  
  var logDir: File = null
  
  override def setUp() {
    logDir = TestUtils.tempDir()
  }
  
  override def tearDown() {
    Utils.rm(logDir)
  }
  
  def createEmptyLogs(dir: File, offsets: Int*) = {
    for(offset <- offsets)
    	new File(dir, Integer.toString(offset) + Log.FILE_SUFFIX).createNewFile()
  }
  
  def testLoadEmptyLog() {
    createEmptyLogs(logDir, 0)
    new Log(logDir, 1024, 1000)
  }
  
  def testLoadInvalidLogsFails() {
    createEmptyLogs(logDir, 0, 15)
    try {
      new Log(logDir, 1024, 1000)
      fail("Allowed load of corrupt logs without complaint.")
    } catch {
      case e: IllegalStateException => "This is good"
    }
  }
  
  def testAppendAndRead() {
    val log = new Log(logDir, 1024, 1000)
    val message = new Message(Integer.toString(42).getBytes())
    for(i <- 0 until 10)
      log.append(new ByteBufferMessageSet(message))
    log.flush()
    val messages = log.read(0, 1024)
    var current = 0
    for(curr <- messages) {
      assertEquals("Read message should equal written", message, curr)
      current += 1
    }
    assertEquals(10, current)
  }
  
  def testReadOutOfRange() {
    createEmptyLogs(logDir, 1024)
    val log = new Log(logDir, 1024, 1000)
    assertEquals("Reading just beyond end of log should produce 0 byte read.", 0L, log.read(1024, 1000).sizeInBytes)
    try {
      log.read(0, 1024)
      fail("Expected exception on invalid read.")
    } catch {
      case e: OffsetOutOfRangeException => "This is good."
    }
    try {
      log.read(1025, 1000)
      fail("Expected exception on invalid read.")
    } catch {
      case e: OffsetOutOfRangeException => "This is good."
    }
  }
  
  /** Test that writing and reading beyond the log size boundary works */
  def testLogRolls() {
    /* create a multipart log with 100 messages */
    val log = new Log(logDir, 100, 1000)
    val numMessages = 100
    for(i <- 0 until numMessages)
      log.append(TestUtils.singleMessageSet(Integer.toString(i).getBytes()))
    log.flush
    
    /* now do successive reads and iterate over the resulting message sets counting the messages
     * we should find exact 100 messages.
     */
    var reads = 0
    var current = 0
    var offset = 0L
    while(current < numMessages) {
      val messages = log.read(offset, 1024*1024)
      for(message <- messages) {
        current += 1
        offset += MessageSet.entrySize(message)
      }
      if(reads > 2*numMessages)
        fail("Too many read attempts.")
      reads += 1
    }
    assertEquals("We did not find all the messages we put in", numMessages, current)
  }
  
  def testFindSegment() {
    assertEquals("Search in empty segments list should find nothing", None, Log.findRange(makeRanges(), 45))
    assertEquals("Search in segment list just outside the range of the last segment should find nothing",
                 None, Log.findRange(makeRanges(5, 9, 12), 12))
    try {
      Log.findRange(makeRanges(35), 36)
      fail("expect exception")
    }
    catch {
      case e: OffsetOutOfRangeException => "this is good"
    }

    try {
      Log.findRange(makeRanges(35,35), 36)
    }
    catch {
      case e: OffsetOutOfRangeException => "this is good"
    }

    assertContains(makeRanges(5, 9, 12), 11)
    assertContains(makeRanges(5), 4)
    assertContains(makeRanges(5,8), 5)
    assertContains(makeRanges(5,8), 6)
  }
  
  /** Test corner cases of rolling logs */
  def testEdgeLogRolls() {
    {
      // first test a log segment starting at 0
      val log = new Log(logDir, 100, 1000)
      val curOffset = log.nextAppendOffset
      assertEquals(curOffset, 0)

      // time goes by; the log file is deleted
      log.markDeletedWhile(_ => true)

      // we now have a new log; the starting offset of the new log should remain 0
      assertEquals(curOffset, log.nextAppendOffset)
    }

    {
      // second test an empty log segment starting at none-zero
      val log = new Log(logDir, 100, 1000)
      val numMessages = 1
      for(i <- 0 until numMessages)
        log.append(TestUtils.singleMessageSet(Integer.toString(i).getBytes()))

      val curOffset = log.nextAppendOffset
      // time goes by; the log file is deleted
      log.markDeletedWhile(_ => true)

      // we now have a new log
      assertEquals(curOffset, log.nextAppendOffset)

      // time goes by; the log file (which is empty) is deleted again
      log.markDeletedWhile(_ => true)

      // we now have a new log
      assertEquals(curOffset, log.nextAppendOffset)
    }
  }

  def assertContains(ranges: Array[Range], offset: Long) = {
    Log.findRange(ranges, offset) match {
      case Some(range) => 
        assertTrue(range + " does not contain " + offset, range.contains(offset))
      case None => fail("No range found, but expected to find " + offset)
    }
  }
  
  class SimpleRange(val start: Long, val size: Long) extends Range
  
  def makeRanges(breaks: Int*): Array[Range] = {
    val list = new ArrayList[Range]
    var prior = 0
    for(brk <- breaks) {
      list.add(new SimpleRange(prior, brk - prior))
      prior = brk
    }
    list.toArray(new Array[Range](list.size))
  }
  
}
