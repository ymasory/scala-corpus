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

import java.net.URL
import com.google.gdata.{Query, Service}


/**
 * This class takes care of the communication with the you tube service and provides
 * methods for retrieving and navigating between different feeds.
 * 
 * Example:
 * <code>
 *   val s = new YouTubeService("mycompany-test-1.0")
 *   for (e <- s.getVideos(Query.empty / "Comedy")) println(e.rating)
 * </code>
 * It will print all the ratings for videos retrieved by the given query (those
 * having the 'Comedy' category).
 * 
 * @author Iulian Dragos
 */
class YouTubeService(appName: String) extends Service(appName, "youtube") {

  /** The video feed module. It depends on the comments feed defined below. */
  val videos = new StdVideoFeed {
    override val commentsFeed: YouTubeService.this.comments.type = comments
  }
  
  /** A standard comments feed, used by videos and playlists. */
  val comments = new StdCommentsFeed
  
  /** A contacts feed. */
  val contacts = new StdContactsFeed
  
  /** A playlist feed. It depends on the comments feed defined above. */
  val playlist = new StdPlaylistFeed {
    override val commentsFeed: YouTubeService.this.comments.type = comments
  }
  
  /** A subscription feed. It uses the video feed of this service. */
  val subscription = new StdSubscriptionFeed {
    override val videoFeeds: YouTubeService.this.videos.type = videos
  }
  
  /** User playlist feed. */
  val userPlaylists = new StdUserPlaylistsFeed {
    override val playlistFeed: YouTubeService.this.videos.type = videos
  }
  
  /** A user profile feed. */ 
  val userProfile = new StdUserProfileEntry
  
  /** Return a video feed matching the given query. */
  def getVideos(q: Query): videos.Feed = {
    query(q.mkUrl(YouTubeService.BASE_VIDEO_FEED), videos.feedPickler)
  }
  
  /** Return a video feed from the given url. */
  def getVideos(url: String): videos.Feed = {
    query(url, videos.feedPickler)
  }
  
  /** Get the comment feed from the given url. */
  def getComments(url: String): comments.Feed = {
    query(url, comments.feedPickler)
  }
  
  /** Get comments for the given video entry. */
  def getComments(ventry: videos.Entry): Option[comments.Feed] = {
    for (comm <- ventry.comments; 
         feedLink <- comm.feedLink)
      yield fromFeedLink(feedLink, comments.feedPickler)
  }
  
  /** Get videos related to the given video. */
  def getRelatedVideos(ventry: videos.Entry): Option[videos.Feed] = {
    for (link <- ventry.link(Schemas.RELATED_VIDEOS))
      yield getVideos(link.href)
  }

  /** Get videos responses to the given video. */
  def getResponseVideos(ventry: videos.Entry): Option[videos.Feed] = {
    for (link <- ventry.link(Schemas.RESPONSES_VIDEOS))
      yield getVideos(link.href)
  }

  /** Get the user profile. */
  def getUserProfile(username: String): userProfile.Entry = {
    query(YouTubeService.USER_PROFILE_FEED + "/" + username, userProfile.entryPickler)
  }
  
  /** Return user's favorite feeds. */
  def getFavorites(username: String): videos.Feed = {
    query(YouTubeService.USER_PROFILE_FEED + "/" + username + "/favorites", 
          videos.feedPickler)
  }

  /** Return user's playlists */
  def getPlaylists(username: String): userPlaylists.Feed = {
    query(YouTubeService.USER_PROFILE_FEED + "/" + username + "/playlists", 
          userPlaylists.feedPickler)
  }

  /** Return user's subscriptions. */
  def getSubscriptions(username: String): subscription.Feed = {
    query(YouTubeService.USER_PROFILE_FEED + "/" + username + "/subscriptions", 
          subscription.feedPickler)
  }
  
  /** Return a user's contacts. */
  def getContacts(username: String): contacts.Feed = {
    query(YouTubeService.USER_PROFILE_FEED + "/" + username + "/contacts", 
          contacts.feedPickler)
  }
}

object YouTubeService {
  final val BASE_VIDEO_FEED = "http://gdata.youtube.com/feeds/api/videos"
  final val USER_PROFILE_FEED = "http://gdata.youtube.com/feeds/api/users"
}
