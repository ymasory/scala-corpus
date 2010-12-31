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

package kafka.network

import java.nio._
import java.nio.channels._
import org.apache.log4j.Logger

/**
 * Represents a stateful transfer of data to or from the network
 */
trait Transmission {
  
  protected val logger: Logger = Logger.getLogger(getClass())
  
  def complete: Boolean
  
  protected def expectIncomplete(): Unit = {
    if(complete)
      throw new IllegalStateException("This operation cannot be completed on a complete request.")
  }
  
  protected def expectComplete(): Unit = {
    if(!complete)
      throw new IllegalStateException("This operation cannot be completed on an incomplete request.")
  }
  
}

/**
 * A transmission that is being received from a channel
 */
trait Receive extends Transmission {
  
  def buffer: ByteBuffer
  
  def readFrom(channel: ReadableByteChannel): Int
  
  def readCompletely(channel: ReadableByteChannel): Int = {
    var read = 0
    while(!complete) {
      read = readFrom(channel)
      if(logger.isTraceEnabled)
        logger.trace(read + " bytes read.")
    }
    read
  }
  
}

/**
 * A transmission that is being sent out to the channel
 */
trait Send extends Transmission {
    
  def writeTo(channel: WritableByteChannel): Int
  
  def writeCompletely(channel: WritableByteChannel): Int = {
    var written = 0
    while(!complete) {
      written = writeTo(channel)
      if(logger.isTraceEnabled)
        logger.trace(written + " bytes written.")
    }
    written
  }
    
}

/**
 * A set of composite sends, sent one after another
 */
class MultiSend[S <: Send](val sends: List[S]) extends Send {
  
  private var current = sends
  
  def writeTo(channel: WritableByteChannel): Int = {
	  expectIncomplete
    val written = current.head.writeTo(channel)
    if(current.head.complete)
      current = current.tail
    written
  }
  
  def complete = current == Nil

}
