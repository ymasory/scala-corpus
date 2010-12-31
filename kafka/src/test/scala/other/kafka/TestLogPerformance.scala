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

package kafka

import kafka.log._
import kafka.message._
import kafka.utils._

object TestLogPerformance {

  def main(args: Array[String]): Unit = {
    if(args.length < 3)
      Utils.croak("USAGE: java " + getClass().getName() + " num_messages message_size batch_size")
    val numMessages = args(0).toInt
    val messageSize = args(1).toInt
    val batchSize = args(2).toInt
    val dir = TestUtils.tempDir()
    val log = new Log(dir, 50*1024*1024, 5000000)
    val bytes = new Array[Byte](messageSize)
    new java.util.Random().nextBytes(bytes)
    val message = new Message(bytes)
    val messages = new Array[Message](batchSize)
    for(i <- 0 until batchSize)
      messages(i) = message
    val messageSet = new ByteBufferMessageSet(messages: _*)
    val numBatches = numMessages / batchSize
    val start = System.currentTimeMillis()
    for(i <- 0 until numBatches)
      log.append(messageSet)
    log.close()
    val ellapsed = (System.currentTimeMillis() - start) / 1000.0
    val writtenBytes = MessageSet.entrySize(message) * numMessages
    println("message size = " + MessageSet.entrySize(message))
    println("MB/sec: " + writtenBytes / ellapsed / (1024.0 * 1024.0))
    Utils.rm(dir)
  }
  
}
