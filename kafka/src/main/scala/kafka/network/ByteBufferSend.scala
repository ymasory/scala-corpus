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
import kafka.utils._

@nonthreadsafe
class ByteBufferSend(val buffer: ByteBuffer) extends Send {
  
  var complete: Boolean = false

  def this(size: Int) = this(ByteBuffer.allocate(size))
  
  def writeTo(channel: WritableByteChannel): Int = {
    expectIncomplete()
    var written = 0
    written += channel.write(buffer)
    if(!buffer.hasRemaining)
      complete = true
    written
  }
    
}
