/*
 * Copyright 2009 Twitter, Inc.
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

import com.twitter.actors.{Actor, Future}

object BulletProofFuture {
  def future[T](body: => T): Future[T] = {
    case object Eval
    case class BlewUp(t: Throwable)

    val a = Actor.actor {
      Actor.react {
        case Eval =>
          try {
            Actor.reply(body)
          } catch {
            case e: Throwable =>
              Actor.reply(BlewUp(e))
          }
      }
    }

    a !! (Eval, {
      case BlewUp(e) => throw e
      case any => any.asInstanceOf[T]
    })
  }
}
