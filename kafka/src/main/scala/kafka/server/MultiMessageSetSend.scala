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

/**
 * A set of message sets prefixed by size
 */
@nonthreadsafe
class MultiMessageSetSend(val sets: List[MessageSetSend]) extends MultiSend(new ByteBufferSend(6) :: sets) {
  
  val buffer = this.sends.head.asInstanceOf[ByteBufferSend].buffer
  buffer.putInt(2 + sets.foldLeft(0)(_ + _.sendSize))
  buffer.putShort(0)
  buffer.rewind()
  
}