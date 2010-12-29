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


package com.google.gdata.youtube;

import com.google.gdata.data.{AtomEntries, AtomFeeds, Entries, Uris}
import com.google.gdata.data.media.{Text, MediaRss}
import com.google.gdata.data.media
import com.google.gdata.data.kinds.{Comments}
import com.google.gdata.data.kinds
import com.google.xml.combinators.{~}
import com.google.xml.combinators.Picklers._

/**
 * A component for video entries. It requires a comments feed instance, and an
 * implementation of MediaRss. It refines the media rss content to support 
 * youtube specific attributes.
 * 
 * @author Iulian Dragos
 */
trait VideoEntries extends AtomEntries {  this: VideoEntries with MediaRss =>
  type Entry <: VideoEntry
  type Content <: YouTubeContent
  
  val commentsFeed: AtomFeeds
  
  /** A pickler for youtube content. */
  protected def ytContentContentsPickler: Pickler[YouTubeContent] =
    (wrap (baseContentPickler ~ attr("format", intVal, Uris.ytNs))
        ({case bc ~ f => 
           val c = new YouTubeContent()
           c.fromBaseContent(bc)
           c.fillOwnFields(f)
           c
        })
        (fromYtContent))
  
  private def fromYtContent(c: YouTubeContent) = new ~(c, c.format)
  
  /**
   * Youtube content adds an yt:format attribute to Media RSS media:content element.
   */
  class YouTubeContent extends BaseContent {
    /** Video format */
    var format: Int = 1
    
    def fillOwnFields(f: Int) = {
      this.format = f
      this
    }
    
    def fromYouTubeContent(c: YouTubeContent) {
      this.fromBaseContent(c)
      this.format = c.format
    }
    
    override def toString: String = {
      super.toString + " format: " + format
    }
  }
  
  /** A YouTubeGrop adds a duration field to the media:group element. */
  class YouTubeGroup extends BaseGroup {
    /** Video length in seconds. */
    var duration: Option[Int] = None
    
    def fillOwnFields(d: Option[Int]) = {
      duration = d
      this
    }
    
    def fromYouTubeGroup(yt: YouTubeGroup) {
      fillOwnFields(yt.duration)
    }
  }
  
  def ytGroupContentsPickler: Pickler[YouTubeGroup] =
    (wrap (baseGroupPickler ~ opt(elem("duration", attr("seconds", intVal))(Uris.ytNs)))
        ({ case bg ~ d => 
             val ytg = new YouTubeGroup
             ytg.fromBaseGroup(bg)
             ytg.fillOwnFields(d) 
        }) (fromYtGroup))
  
  private def fromYtGroup(g: YouTubeGroup) = new ~(g, g.duration)
  
  /**
   * A Media entry adds MediaRss group elements. 
   * 
   * @author Iulian Dragos
   */
  class VideoEntry extends AtomEntry {
    /** Media Rss group. */
    var media: Group = _
    
    /** When 'true', this video can't be embedded. */
    var noembed: Boolean = false
    
    /** Video contains restricted content (yt:racy). */
    var restricted: Boolean = false
    
    /** The number of times this video has been viewed. */
    var viewCount: Int = 0
    
    /** Video rating. */
    var rating: Option[kinds.Rating] = None
    
    /** Comments feed for this entry. */
    var comments: Option[Comments[commentsFeed.Feed]] = None 
    
    def fillOwnFields(media: Group, noembed: Boolean, restricted: Boolean,
        viewCount: Int, rating: Option[kinds.Rating], 
        comments: Option[Comments[commentsFeed.Feed]]): this.type = {
      this.media = media
      this.noembed = noembed
      this.restricted = restricted
      this.viewCount = viewCount
      this.rating = rating
      this.comments = comments
      this
    }
    
    def fromVideoEntry(me: VideoEntry) {
      this.fromAtomEntry(me)
      fillOwnFields(me.media, me.noembed, me.restricted, me.viewCount, me.rating, me.comments)
    }
    
    override def toString = {
      super.toString + " media: " + media + " noembed: " + noembed + " restricted: " +
          restricted + " viewCount: " + viewCount + " rating: " + rating +
          " comments: " + comments
    }
  }
  
  /** Additional elements defiend by video entries. */
  lazy val videoEntryExtra = interleaved((groupPickler ~ marker(elem("noembed", text)(Uris.ytNs))
      ~ marker(elem("racy", text)(Uris.ytNs))
      ~ default(elem("statistics", attr("viewCount", intVal))(Uris.ytNs), 0)
      ~ opt(kinds.Rating.pickler)
      ~ opt(Comments.pickler(commentsFeed.feedPickler))))
    
  /**
   * A pickler for media entries. It pickles/unpickles just the contents of an entry. 
   */
  def videoEntryContentsPickler: Pickler[VideoEntry] =
    wrap (atomEntryContentsPickler ~ videoEntryExtra) ({
      case ae ~ (media ~ noembed ~ racy ~ vc ~ rating ~ comments) => 
        val me = new VideoEntry
        me.fromAtomEntry(ae)
        me.fillOwnFields(media, noembed, racy, vc, rating, comments)
    }) (fromVideoEntry)

  private def fromVideoEntry(me: VideoEntry) = 
    new ~(me, new ~(me.media, me.noembed) ~ me.restricted ~ me.viewCount ~ me.rating ~ me.comments)
}
