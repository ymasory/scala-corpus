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


package com.google
package gdata.youtube

import com.google.gdata.data.{AtomEntries, AtomFeeds, Uris}
import com.google.gdata.data.kinds.FeedLink
import com.google.gdata.data.media.Thumbnail
import com.google.xml.combinators.{Picklers, ~}

trait UserProfileEntries extends AtomEntries {
  type Entry <: UserProfileEntry
  
  /** 
   * Abstract value for video feeds. Subclasses should instantiate it to the desired
   * video feeds.
   */
  val videoFeeds: VideoFeeds
  
  /**
   * Abstract value for playlist feeds. Subclasses should instantiate it to the desired
   * playlist feed object.
   */
  val playlistFeeds: UserPlaylistsFeed
  
  /**
   * Abstract value for contact feeds. Subclasses should instantiate it to the
   * desired implementation of contact feeds.
   */
  val contactFeeds: AtomFeeds with ContactsEntries
  
  val subscriptionFeeds: AtomFeeds with SubscriptionEntries

  /**
   * A user profile entry in user's profile feed.
   * 
   * @author Iulian Dragos
   */
  class UserProfileEntry extends AtomEntry {
    var username: String               = ""
    var firstName: Option[String]      = None
    var lastName: Option[String]       = None
    var age: Option[Int]               = None
    var books: Option[String]          = None
    var gender: Option[String]         = None
    var company: Option[String]        = None
    var description: Option[String]    = None
    var hobbies: Option[String]        = None
    var hometown: Option[String]       = None
    var location: Option[String]       = None
    var movies: Option[String]         = None
    var music: Option[String]          = None
    var occupation: Option[String]     = None
    var school: Option[String]         = None
    var relationship: Option[String]   = None
    var statistics: Option[Statistics] = None
    var thumbnail: Option[Thumbnail]   = None
    
    var favorites: FeedLink[videoFeeds.Feed] = _
    var playlists: FeedLink[playlistFeeds.Feed] = _
    var uploads: FeedLink[videoFeeds.Feed] = _

    var contacts: FeedLink[contactFeeds.Feed] = _
    var subscriptions: FeedLink[subscriptionFeeds.Feed] = _
    
    def fromUserProfileEntry(pe: UserProfileEntry) {
      fromAtomEntry(pe)
      fillOwnFields(pe.username, pe.firstName, pe.lastName, pe.age, pe.books, pe.gender,
          pe.company, pe.description, pe.hobbies, pe.hometown, pe.location, pe.movies,
          pe.music, pe.occupation, pe.school, pe.relationship, pe.statistics, pe.thumbnail,
          pe.favorites, pe.playlists, pe.uploads, pe.contacts, pe.subscriptions)
    }
    
    /** Fill fields defined by this class. */
    def fillOwnFields(username: String, firstName: Option[String], lastName: Option[String], 
        age: Option[Int], books: Option[String], gender: Option[String], company: Option[String], 
        description: Option[String], hobbies: Option[String], hometown: Option[String],
        location: Option[String], movies: Option[String], music: Option[String],
        occupation: Option[String], school: Option[String], relationship: Option[String],
        statistics: Option[Statistics], thumbnail: Option[Thumbnail], 
        favorites: FeedLink[videoFeeds.Feed], playlists: FeedLink[playlistFeeds.Feed],
        uploads: FeedLink[videoFeeds.Feed], contacts: FeedLink[contactFeeds.Feed],
        subscriptions: FeedLink[subscriptionFeeds.Feed]) = {
      this.username = username
      this.firstName = firstName
      this.lastName = lastName
      this.age = age
      this.books = books
      this.gender = gender
      this.company = company
      this.description = description
      this.hobbies = hobbies
      this.hometown = hometown
      this.location = location
      this.movies = movies
      this.music = music
      this.occupation = occupation
      this.school = school
      this.relationship = relationship
      this.statistics = statistics
      this.thumbnail = thumbnail
      this.favorites = favorites
      this.playlists = playlists
      this.uploads = uploads
      this.contacts = contacts
      this.subscriptions = subscriptions
      this
    }
    
    override def toString = {
      import util.Utility.printOptional
      
      val sb = new StringBuilder
      sb.append(super.toString).append("\nUsername: ").append(username)
      printOptional(sb, "\nfirstName", firstName)
      printOptional(sb, "\nlastName", lastName)
      printOptional(sb, "\nage", age)
      printOptional(sb, "\nbooks", books)
      printOptional(sb, "\ngender", gender)
      printOptional(sb, "\ncompany", company)
      printOptional(sb, "\ndescription", description)
      printOptional(sb, "\nhobbies", hobbies)
      printOptional(sb, "\nhometown", hometown)
      printOptional(sb, "\nlocation", location)
      printOptional(sb, "\nmovies", movies)
      printOptional(sb, "\nmusic", music)
      printOptional(sb, "\noccupation", occupation)
      printOptional(sb, "\nschool", school)
      printOptional(sb, "\nrelationship", relationship)
      printOptional(sb, "\nstatistics", statistics)
      printOptional(sb, "\nthumbnail", thumbnail)
      sb.append("\nfavorites").append(favorites)
      sb.append("\nplaylists").append(playlists)
      sb.append("\nuploads").append(uploads)
      sb.append("\ncontacts").append(contacts)
      sb.append("\nsubscription").append(subscriptions)
      sb.toString
    }
  }

  
  def userProfileEntryContents: Picklers.Pickler[UserProfileEntry] = {
    import Picklers._
    implicit val ns = Uris.ytNs
    
    /* A recognizer for a particular feed link. */
    def feedLinkRel(rel: String) = {
      elem("feedLink", const(attr("rel", text), rel))(Uris.gdNs)
    }
    
    val extraContents = {
      /** Convenience method for an optional text element. */
      def ote(label: String)(implicit ns: (String, String)): Pickler[Option[String]] =
        opt(elem(label, text))
  
      interleaved(elem("username", text) ~ ote("firstName") ~ ote("lastName")
        ~ opt(elem("age", intVal)) ~ ote("books") ~ ote("gender") ~ ote("company")
        ~ ote("description") ~ ote("hobbies") ~ ote("hometown") ~ ote("location")
        ~ ote("movies") ~ ote("music") ~ ote("occupation") ~ ote("school") ~ ote("relationship")
        ~ opt(Statistics.pickler) ~ opt(Thumbnail.pickler)
        ~ when(feedLinkRel(Schemas.USER_FAVORITES), FeedLink.pickler(videoFeeds.feedPickler))
        ~ when(feedLinkRel(Schemas.USER_PLAYLISTS), FeedLink.pickler(playlistFeeds.feedPickler))
        ~ when(feedLinkRel(Schemas.USER_UPLOADS), FeedLink.pickler(videoFeeds.feedPickler)) 
        ~ when(feedLinkRel(Schemas.USER_CONTACTS), 
              FeedLink.pickler(contactFeeds.feedPickler))
        ~ when(feedLinkRel(Schemas.USER_SUBSCRIPTIONS), 
              FeedLink.pickler(subscriptionFeeds.feedPickler))) 
    }
    
    def fromUserProfileEntry(pe: UserProfileEntry) = new ~(pe, new ~(pe.username, pe.firstName)
        ~ pe.lastName ~ pe.age ~ pe.books ~ pe.gender ~ pe.company ~ pe.description ~ pe.hobbies
        ~ pe.hometown ~ pe.location ~ pe.movies ~ pe.music ~ pe.occupation ~ pe.school
        ~ pe.relationship ~ pe.statistics ~ pe.thumbnail ~ pe.favorites ~ pe.playlists
        ~ pe.uploads ~ pe.contacts ~ pe.subscriptions)

    wrap (atomEntryContentsPickler ~ extraContents) {
      case  ae ~ (username ~ firstName ~ lastName ~ age ~ books ~ gender ~ company ~ description
          ~ hobbies ~ hometown ~ location ~ movies ~ music ~ occupation ~ school ~ relationship 
          ~ statistics ~ thumbnail ~ favorites ~ playlists ~ uploads ~ contacts ~ subscriptions) =>
        val profileEntry = new UserProfileEntry
        profileEntry.fromAtomEntry(ae)
        profileEntry.fillOwnFields(username, firstName, lastName, age, books, gender, company, 
          description, hobbies, hometown, location, movies, music, occupation, school, relationship,
          statistics, thumbnail, favorites, playlists, uploads, contacts, subscriptions)
    } (fromUserProfileEntry)
  }
  

}
