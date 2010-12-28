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

import org.apache.mina.core.buffer.IoBuffer
import org.apache.mina.core.session.IoSession
import org.apache.mina.filter.codec.{ProtocolCodecFilter, ProtocolEncoder, ProtocolEncoderOutput}
import net.lag.extensions._
import net.lag.naggati.{Decoder, End, ProtocolError}
import net.lag.naggati.Steps._


abstract sealed class MemcacheResponse

object MemcacheResponse {
  // 3 types of error that can be returned by the server
  case object Error extends MemcacheResponse
  case class ClientError(reason: String) extends MemcacheResponse
  case class ServerError(reason: String) extends MemcacheResponse

  // when fetching stats or values:
  case class Value(key: String, flags: Int, casKey: String, data: Array[Byte]) extends MemcacheResponse {
    // FIXME: why doesn't scala do array equals correctly?
    override def equals(obj: Any) = {
      obj match {
        case Value(okey, oflags, ocasKey, odata) =>
          (key == okey) && (flags == oflags) && (casKey == ocasKey) &&
            (java.util.Arrays.equals(data, odata))
        case _ => false
      }
    }
  }
  case class StatItem(key: String, value: String) extends MemcacheResponse
  case object EndOfResults extends MemcacheResponse

  // set/add/replace/append/prepend/cas/delete/incr/decr:
  case object NotFound extends MemcacheResponse

  // set/add/replace/append/prepend/cas:
  case object Stored extends MemcacheResponse
  case object NotStored extends MemcacheResponse
  case object Exists extends MemcacheResponse

  // delete:
  case object Deleted extends MemcacheResponse

  // incr/decr:
  case class NewValue(data: Long) extends MemcacheResponse
}


// naggati state machine generator
object MemcacheClientDecoder {
  val response = readLine { line =>
    val parts = line.split(" ", 2)
    parts(0) match {
      case "ERROR" =>
        state.out.write(MemcacheResponse.Error)
        End

      case "CLIENT_ERROR" =>
        state.out.write(MemcacheResponse.ClientError(if (parts.length == 2) parts(1) else "(unknown)"))
        End

      case "SERVER_ERROR" =>
        state.out.write(MemcacheResponse.ServerError(if (parts.length == 2) parts(1) else "(unknown)"))
        End

      case "NOT_FOUND" =>
        state.out.write(MemcacheResponse.NotFound)
        End

      case "STORED" =>
        state.out.write(MemcacheResponse.Stored)
        End

      case "NOT_STORED" =>
        state.out.write(MemcacheResponse.NotStored)
        End

      case "EXISTS" =>
        state.out.write(MemcacheResponse.Exists)
        End

      case "DELETED" =>
        state.out.write(MemcacheResponse.Deleted)
        End

      case "END" =>
        state.out.write(MemcacheResponse.EndOfResults)
        End

      case "STAT" =>
        val subParts = parts(1).split(" ")
        if (subParts.length < 2) {
          throw new ProtocolError("Corrupted STAT line")
        }
        try {
          val name  = subParts(0)
          val value = subParts(1)
          state.out.write(MemcacheResponse.StatItem(name, value))
          End
        } catch {
          case e: Exception =>
          throw new ProtocolError("Corrupted STAT line")
        }

      case "VALUE" =>
        val subParts = parts(1).split(" ")
        if ((subParts.length < 3) || (subParts.length > 4)) {
          throw new ProtocolError("Corrupted VALUE line")
        }
        try {
          val key = subParts(0)
          val flags = subParts(1).toInt
          val bytes = subParts(2).toInt
          val casKey = if (subParts.length == 4) subParts(3) else ""
          readByteBuffer(bytes) { bytes =>
            state.out.write(MemcacheResponse.Value(key, flags, casKey, bytes))
            readByteBuffer(2) { lf =>
              if (new String(lf, "UTF-8") != "\r\n") {
                throw new ProtocolError("Corrupted VALUE terminator: " + lf.hexlify)
              }
              End
            }
          }
        } catch {
          case e: NumberFormatException =>
            throw new ProtocolError("Corrupted VALUE line")
        }

      // incr/decr response:
      case value =>
        try {
          state.out.write(MemcacheResponse.NewValue(value.toLong))
          End
        } catch {
          case e: NumberFormatException =>
            throw new ProtocolError("Unexpected response: " + value)
        }
    }
  }


  val decoder = new Decoder(response)
  val encoder = new ProtocolEncoder {
    def encode(session: IoSession, message: AnyRef, out: ProtocolEncoderOutput) = {
      val data = message match {
        case s: String => s.getBytes
        case b: Array[Byte] => b
      }
      out.write(IoBuffer.wrap(data))
    }

    def dispose(session: IoSession): Unit = {
      // nothing.
    }
  }
  val filter = new ProtocolCodecFilter(encoder, decoder)
}
