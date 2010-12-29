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


package com.google.gdata.youtube

import com.google.gdata.data.{AtomEntries, Uris}
import com.google.gdata.data.kinds.FeedLink
import com.google.xml.combinators.{Picklers, ~}

/**
 * User Subscription entries extends atom entries with subscription-specific elements
 * like 'queryString' and 'feedLink'.
 * 
 * @author Iulian Dragos
 */
trait SubscriptionEntries extends AtomEntries {
  type Entry <: SubscriptionEntry
  
  /** The video feeds module. Subclasses should initialize this to the desired video feed type. */
  val videoFeeds: VideoFeeds
  
  /**
   * User subscription entries. A subscription can be a channel, favorites or query
   * subscription. The feed link points to the target of this subscription.
   * 
   * @author Iulian Dragos
   */
  class SubscriptionEntry extends AtomEntry {
    /** A link to the subscribed feed. */
    var feedLink: FeedLink[videoFeeds.Feed] = _
    
    /** The username for a 'channel' or 'favorites' subscription. */
    var username: Option[String] = None
    
    /** The query string for a query subscription. */
    var queryString: Option[String] = None
    
    def fromSubscriptionEntry(se: SubscriptionEntry) = {
      fromAtomEntry(se)
      fillOwnFields(se.feedLink, se.username, se.queryString)
    }
    
    def fillOwnFields(feedLink: FeedLink[videoFeeds.Feed], username: Option[String],
        queryString: Option[String]): this.type = {
      this.feedLink = feedLink
      this.username = username
      this.queryString = queryString
      this
    }
  }
  
  /** A pickler for subscription entries. */
  protected def subscriptionEntryContentsPickler = {
    implicit val ns = Uris.ytNs
    import Picklers._
    
    val contents = interleaved(atomEntryContentsPickler
        ~ FeedLink.pickler(videoFeeds.feedPickler) ~ opt(elem("username", text))
        ~ opt(elem("queryString", text)))
    
    def fromSubscriptionEntry(se: SubscriptionEntry) =
      new ~(se, se.feedLink) ~ se.username ~ se.queryString
    
    wrap (contents) {
      case ae ~ feedLink ~ username ~ queryString =>
        val se = new SubscriptionEntry
        se.fromAtomEntry(ae)
        se.fillOwnFields(feedLink, username, queryString)
    } (fromSubscriptionEntry)
  }
}
