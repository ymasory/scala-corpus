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

package kafka.message

import java.util._
import java.nio._
import java.nio.channels._
import java.io._
import junit.framework.TestCase
import junit.framework.Assert._
import kafka.TestUtils

class MessageTest extends TestCase {
  
  var message: Message = null
  val payload = "some bytes".getBytes()

  override def setUp(): Unit = {
    message = new Message(payload)
  }
  
  def testFieldValues = {
    TestUtils.checkEquals(ByteBuffer.wrap(payload), message.payload)
    assertEquals(Message.CurrentMagicValue, message.magic)
    assertEquals(69L, new Message(69, "hello".getBytes()).checksum)
  }

  def testChecksum() {
    assertTrue("Auto-computed checksum should be valid", message.isValid)
    val badChecksum = message.checksum + 1 % Int.MaxValue
    val invalid = new Message(badChecksum, payload)
    assertEquals("Message should return written checksum", badChecksum, invalid.checksum)
    assertFalse("Message with invalid checksum should be invalid", invalid.isValid)
  }
  
  def testEquality() = {
    assertFalse("Should not equal null", message.equals(null))
    assertFalse("Should not equal a random string", message.equals("asdf"))
    assertTrue("Should equal itself", message.equals(message))
    val copy = new Message(message.checksum, payload)
    assertTrue("Should equal another message with the same content.", message.equals(copy))
  }
  
  def testIsHashable() = {
    // this is silly, but why not
    val m = new HashMap[Message,Boolean]()
    m.put(message, true)
    assertNotNull(m.get(message))
  }
  
}
