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

/**
 * A concrete, standard video feed. It turns all types in VideoFeeds to concrete types.
 * Feed is an atom feed, entries are video entries, and the media group content has the
 * youtube elements.
 * 
 * @see AtomFeed, VideoEntry, YouTubeGroup, YouTubeContent
 * @author Iulian Dragos
 */
class StdVideoFeed extends VideoFeeds {
  type Entry   = VideoEntry
  type Feed    = AtomFeed
  type Content = YouTubeContent
  type Group   = YouTubeGroup
  
  protected def groupContentsPickler = ytGroupContentsPickler
  protected def contentContentsPickler = ytContentContentsPickler

  val commentsFeed = new StdCommentsFeed

  protected def entryContentsPickler = videoEntryContentsPickler
  protected def feedContentsPickler = atomFeedContentsPickler
}
