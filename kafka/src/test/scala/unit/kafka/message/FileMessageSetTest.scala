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

import java.nio._
import java.util.Arrays
import junit.framework.TestCase
import junit.framework.Assert._
import kafka.message._
import kafka.TestUtils._

class FileMessageSetTest extends BaseMessageSetTestCases {
  
  val messageSet = createMessageSet(messages)
  
  def createMessageSet(messages: Seq[Message]): FileMessageSet = {
    val set = new FileMessageSet(tempFile(), true)
    set.append(new ByteBufferMessageSet(messages: _*))
    set.flush()
    set
  }
  
  def testFileSize() {
    assertEquals(messageSet.channel.size, messageSet.sizeInBytes)
    messageSet.append(singleMessageSet("abcd".getBytes()))
    assertEquals(messageSet.channel.size, messageSet.sizeInBytes)
  }
  
  def testIterationOverPartialAndTruncation() {
    testPartialWrite(0)
	  testPartialWrite(2)
    testPartialWrite(4)
    testPartialWrite(5)
    testPartialWrite(6)
  }
  
  def testPartialWrite(size: Int) {
    val buffer = ByteBuffer.allocate(size)
    val originalPosition = messageSet.channel.position
    for(i <- 0 until size)
    	buffer.put(0.asInstanceOf[Byte])
    buffer.rewind()
    messageSet.channel.write(buffer)
    // appending those bytes should not change the contents
    checkEquals(messages.iterator, messageSet.iterator)
    assertEquals("Unexpected number of bytes truncated", size.longValue, messageSet.recover())
    assertEquals("File pointer should now be at the end of the file.", originalPosition, messageSet.channel.position)
    // nor should recovery change the contents
    checkEquals(messages.iterator, messageSet.iterator)
  }
  
  def testIterationDoesntChangePosition() {
    val position = messageSet.channel.position
    checkEquals(messages.iterator, messageSet.iterator)
    assertEquals(position, messageSet.channel.position)
  }
  
  def testRead() {
    val read = messageSet.read(0, messageSet.sizeInBytes)
    checkEquals(messageSet.iterator, read.iterator)
    val items = read.iterator.toList
    val first = items.head
    val read2 = messageSet.read(MessageSet.entrySize(first), messageSet.sizeInBytes)
    checkEquals(items.tail.iterator, read2.iterator)
  }
  
}
