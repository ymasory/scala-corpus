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
import Picklers._

/**
 * Users playlist link entries. They contain a link to a video feed containing
 * the videos in the playlist.
 * 
 * @see http://code.google.com/apis/youtube/reference.html#UsersPlaylistFeed
 * 
 * @author Iulian Dragos
 */
trait PlaylistEntries extends AtomEntries {
  type Entry <: PlaylistEntry
  
  /** The video feed for a playlist entry. */
  val playlistFeed: VideoFeeds
  
  /** An abstract pickler for the PlaylistFeed type. @see PlaylistFeed. */
  def playlistFeedPickler: Pickler[playlistFeed.Feed]
  
  /**
   * A user's playlist entry. Adds a yt:private element, yt:description and
   * a feed link for finding the playlist contents.
   * 
   * @author Iulian Dragos
   */
  class PlaylistEntry extends AtomEntry {
    /** Playlist description. */
    var description: Option[String] = None
    
    /** Link to a feed for the playlist videos. */
    var feedLink: Option[FeedLink[playlistFeed.Feed]] = None
    
    def fillOwnFields(description: Option[String], 
        feedLink: Option[FeedLink[playlistFeed.Feed]]) = {
      this.description = description
      this.feedLink = feedLink
      this
    }
    
    override def toString = {
      super.toString + " description: " + description + " feedLink: " + feedLink
    }
  }

  /** A pickler for playlist entries. */
  protected def playlistEntryContentsPickler = 
    (wrap (atomEntryContentsPickler ~ interleaved(opt(elem("description", text)(Uris.ytNs)) 
        ~ opt(FeedLink.pickler(playlistFeedPickler))))
        ({ case ae ~ (desc ~ feedLink) =>
             val plEntry = new PlaylistEntry
             plEntry.fromAtomEntry(ae)
             plEntry.fillOwnFields(desc, feedLink)
        }) (fromPlaylistEntry)
    )
  
  private def fromPlaylistEntry(ple: PlaylistEntry) = 
    new ~(ple, new ~(ple.description, ple.feedLink))
}
