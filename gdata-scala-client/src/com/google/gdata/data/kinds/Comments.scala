/* Copyright (c) 2008 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


package com.google.gdata.data
package kinds

import com.google.xml.combinators.{Picklers, ~}

/**
 * A gd:comments element. Contains a comments feed for the enclosing entry.
 * 
 * @param rel Type of comments contained within.
 * @param feedLink Comments Feed (Should implement the message kind).
 */
case class Comments[Feed](rel: Option[String], feedLink: Option[FeedLink[Feed]])

object Comments {
  import Picklers._
  
  def pickler[F](feedPickler: Pickler[F]): Pickler[Comments[F]] =
    (wrap (elem("comments", opt(attr("rel", text))
         ~ opt(FeedLink.pickler(feedPickler)))(Uris.gdNs))
         { case rel ~ fl => Comments(rel, fl) } (fromComments))
  
  private def fromComments[F](c: Comments[F]) = new ~(c.rel, c.feedLink)
}