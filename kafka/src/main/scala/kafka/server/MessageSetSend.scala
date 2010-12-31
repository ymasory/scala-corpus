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

package kafka.server

import java.nio._
import java.nio.channels._
import kafka.network._
import kafka.message._
import kafka.utils._
import kafka.common.ErrorMapping

/**
 * A zero-copy message response that writes the bytes needed directly from the file
 * wholly in kernel space
 */
@nonthreadsafe
class MessageSetSend(val messages: MessageSet, val errorCode: Int) extends Send {
  
  private var sent: Long = 0
  private var size: Long = messages.sizeInBytes
  private val header = ByteBuffer.allocate(6)
  header.putInt(size.asInstanceOf[Int] + 2)
  header.putShort(errorCode.asInstanceOf[Short])
  header.rewind()
  
  var complete: Boolean = false

  def this(messages: MessageSet) = this(messages, ErrorMapping.NO_ERROR) 

  def writeTo(channel: WritableByteChannel): Int = {
    expectIncomplete()
    var written = 0
    if(header.hasRemaining)
      written += channel.write(header)
    if(!header.hasRemaining) {
      val fileBytesSent = messages.writeTo(channel, sent, size - sent)
      written += fileBytesSent.asInstanceOf[Int]
      sent += fileBytesSent
    }
    
    if(sent >= size)
      complete = true
    written
  }
  
  def sendSize: Int = size.asInstanceOf[Int] + header.capacity
  
}
