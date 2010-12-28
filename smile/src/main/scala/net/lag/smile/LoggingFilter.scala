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

package net.lag.smile

import net.lag.extensions._
import net.lag.logging.Logger
import org.apache.mina.core.buffer.IoBuffer
import org.apache.mina.core.filterchain.{IoFilter, IoFilterAdapter}
import org.apache.mina.core.session.IoSession
import org.apache.mina.core.write.WriteRequest


class LoggingFilter extends IoFilterAdapter {

  private val log = Logger.get

  override def exceptionCaught(nextFilter: IoFilter.NextFilter, session: IoSession,
    cause: Throwable) = {
    log.warning(cause, "exception in filter (session=%s): %s", session, cause.toString)
    nextFilter.exceptionCaught(session, cause)
  }

  override def messageReceived(nextFilter: IoFilter.NextFilter, session: IoSession,
    message: Object) = {
    message match {
      case data: Array[Byte] =>
        log.trace("incoming on session %s:", session)
        logHex("IN  ", data, 0, data.length)
      case buffer: IoBuffer =>
        log.trace("incoming on session %s:", session)
        logHex("IN  ", buffer.array, buffer.position, buffer.limit - buffer.position)
      case other =>
    }
    nextFilter.messageReceived(session, message)
  }

  override def messageSent(nextFilter: IoFilter.NextFilter, session: IoSession,
    writeRequest: WriteRequest) = {
    writeRequest.getMessage match {
      case data: Array[Byte] =>
        if (data.length > 0) {
          log.trace("outgoing on session %s:", session)
          logHex("OUT ", data, 0, data.length)
        }
      case buffer: IoBuffer =>
        if (buffer.limit - buffer.position > 0) {
          log.trace("outgoing on session %s:", session)
          logHex("OUT ", buffer.array, buffer.position, buffer.limit - buffer.position)
        }
      case other =>
    }
    nextFilter.messageSent(session, writeRequest)
  }



  private def logHex(prefix: String, data: Array[Byte], offset: Int, length: Int) = {
    if (length == 0) {
      log.trace("nothing")
      throw new Exception("hetasnhoeua")
    }
    var i = 0
    while (i < length) {
      val n = (length - i) min 16
      logHexLine(prefix, data, offset + i, n)
      i += n
    }
  }

  private def logHexLine(prefix: String, data: Array[Byte], offset: Int, length: Int) = {
    val hex = new StringBuilder
    val chars = new StringBuilder
    for (i <- 0 until length) {
      val b = data(offset + i).toInt & 0xff
      hex append "%02x ".format(b)
      if (i % 8 == 7) {
        hex += ' '
      }
      chars += (if (b > 0x20 && b < 0x80) b.toChar else '.')
    }
    log.trace("%s%04x: %-50s%s", prefix, offset, hex, chars)
  }

/*
  void  sessionClosed(IoFilter.NextFilter nextFilter, IoSession session)
           Filters IoHandler.sessionClosed(IoSession) event.
  void  sessionCreated(IoFilter.NextFilter nextFilter, IoSession session)
           Filters IoHandler.sessionCreated(IoSession) event.
  void  sessionIdle(IoFilter.NextFilter nextFilter, IoSession session, IdleStatus status)
           Filters IoHandler.sessionIdle(IoSession,IdleStatus) event.
  void  sessionOpened(IoFilter.NextFilter nextFilter, IoSession session)
           Filters IoHandler.sessionOpened(IoSession) event.
*/}
