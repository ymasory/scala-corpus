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

import com.google.xml.combinators.{Picklers, HasStore, ~}

/**
 * A gd:feedLink element. It represents a logically nested feed of abstract
 * type 'Feed'.
 * 
 * @author Iulian Dragos
 */
class FeedLink[Feed] extends HasStore {
  /** Hints at the number of entries in the feed. May not be a precise count. */
  var countHint: Option[Int] = None
  
  /** The feed URI. If the feed is embedded, this may be omitted. */
  var href: Option[String] = None
  
  /** Is the contained feed read only? */
  var readOnly: Option[Boolean] = None
  
  /** The link relation. */
  var rel: Option[String] = None
  
  /** The embedded feed. */
  var feed: Option[Feed] = None
  
  def fillOwnFields(countHint: Option[Int], href: Option[String], readOnly: Option[Boolean],
      rel: Option[String], feed: Option[Feed]): this.type = {
    this.countHint = countHint
    this.href = href
    this.readOnly = readOnly
    this.rel = rel
    this.feed = feed
    this
  }
  
  override def toString = {
    import com.google.util.Utility.printOptional
    
    val sb = new StringBuilder
    sb.append("FeedLink: ")
    printOptional(sb, "countHint", countHint)
    printOptional(sb, "href", href)
    printOptional(sb, "rel", rel)
    printOptional(sb, "readOnly", readOnly)
    if (feed.isDefined) sb.append(" and has embedded feed.")
    sb.toString
  }
}

object FeedLink {
  import Picklers._
  
  /** A pickler for FeedLinks with feeds of type F */
  def pickler[F](p: Pickler[F]): Pickler[FeedLink[F]] = {
    elem("feedLink", makeExtensible(contentsPickler(p)))(Uris.gdNs) 
  }
  
  /** A pickler for FeedLinks contents. It has no enclosing 'gd:feedLink' element. */
  def contentsPickler[F](feedPickler: Pickler[F]): Pickler[FeedLink[F]] =
    (wrap (opt(attr("countHint", intVal)) ~ opt(attr("href", text))
        ~ opt(attr("readOnly", boolVal)) ~ opt(attr("rel", text)) ~ opt(feedPickler))
        ((new FeedLink[F]).fillOwnFields _) (fromFeedLink))
  
  private def fromFeedLink[F](fl: FeedLink[F]) = 
    new ~(fl.countHint, fl.href) ~ fl.readOnly ~ fl.rel ~ fl.feed
}
