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


package com.google.gdata
package calendar

import java.net.URL

/**
 * A service class for connecting to the calendar service. It provides
 * methods for getting a user's calendars, managing events and calendar
 * subscriptions.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/calendar/developers_guide_protocol.html
 * @see com.google.gdata.Service
 */
class CalendarService(appName: String) extends Service(appName, "cl")  {
  /** A standard comment feed. */
  val comments = new data.StdAtomFeed
  
  /** A standard contact entry, that might be part of a feed. */
  val contacts = new data.kinds.StdContactEntries
  
  /** An events feed. */
  object eventsFeed extends StdEventsFeed {
    /** Make sure the comments feed used by events is the one used by this Service. */
    override val commentsFeed: comments.type = comments

    /** Make sure the contact entries used by events is the one used by this Service. */
    override val contactEntries: contacts.type = contacts
  }
  
  /** The access control list feed. */
  val aclFeed = new StdAclFeed
  
  /** A calendars feed. */
  object calendarsFeed extends StdCalendarsFeed {
    override val contactEntries: contacts.type = contacts
  }
  
  /**
   * Retrieve calendars found at the given URL.
   * 
   * @throws UnknownDocumentException if the feed is not a calendar feed.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def getCalendars(url: URL): calendarsFeed.Feed = {
    query(url, calendarsFeed.feedPickler)
  }
  
  /**
   * Retrieve calendars found at the given URL.
   * 
   * @throws UnknownDocumentException if the pickler is unsuccessful.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def getCalendars(url: String): calendarsFeed.Feed = {
    query(url, calendarsFeed.feedPickler)
  }
  
  /**
   * Retrieve all user calendars. This is a read/write feed and needs authentication.
   * 
   * @see Service.setUserCredentials
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def getAllUserCalendars: calendarsFeed.Feed = {
    getCalendars(CalendarService.FEEDS + "/default/allcalendars/full")
  }
  
  /**
   * Retrieve calendars that a user owns. This is a read/write feed and needs authentication.
   * 
   * @see Service.setUserCredentials
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def getOwnedUserCalendars: calendarsFeed.Feed = {
    getCalendars(CalendarService.FEEDS + "/default/owncalendars/full")
  }

  /**
   * Add a new calendar subscription to the given calendar id. Returns a calendar entry
   * possibly updated by the server.
   * 
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def addCalendar(id: String): calendarsFeed.Entry = {
    val cal = new calendarsFeed.Entry
    cal.id = Some(id)
    insert(new URL(CalendarService.FEEDS + "/default/allcalendars/full"), 
        cal, calendarsFeed.entryPickler)
  }
  
  /**
   * Update the given calendar subscription.
   * 
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def updateCalendar(url: URL, c: calendarsFeed.Entry): calendarsFeed.Entry = {
    update(url, c, calendarsFeed.entryPickler)
  }
  
  /**
   * Update the given calendar subscription. The entry should have a link with a 'rel'
   * field set to 'edit'. This method will connect to that link and issue an update request.
   * Returns the updated calendar entry.
   * 
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def updateCalendar(c: calendarsFeed.Entry): calendarsFeed.Entry = {
    c.linkHref("edit") match {
      case Some(editUrl) => updateCalendar(new URL(editUrl), c)
      case None => throw new IllegalArgumentException("Calendar has no 'edit' link field.")
    }
  }
  
  /**
   * Retrieve events found at the given URL.
   * 
   * @throws UnknownDocumentException if the feed is not an event feed.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def getEvents(url: String): eventsFeed.Feed =
    getEvents(new URL(url))
  
  /**
   * Retrieve events found at the given URL.
   * 
   * @throws UnknownDocumentException if the feed is not an event feed.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def getEvents(url: URL): eventsFeed.Feed = {
    query(url, eventsFeed.feedPickler)
  }
  
  /**
   * Retrieve events based on their visibility and with the amount of detail given by
   * the 'projection' parameter.
   * 
   * @see http://code.google.com/apis/calendar/reference.html#Projection
   * @see com.google.gdata.calendar.CalendarService
   * @throws UnknownDocumentException if the feed is not an event feed.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def getEvents(visibility: String, projection: String, q: Query): eventsFeed.Feed = {
    if (username.isEmpty)
      throw new IllegalStateException("Set user credentials before querying for users events.")
    getEvents(username.get, visibility, projection, q)
  }
  
  /**
   * Retrieve events for the given user, based on their visibility and with the amount 
   * of detail given by the 'projection' parameter.
   * 
   * @see http://code.google.com/apis/calendar/reference.html#Projection
   * @see com.google.gdata.calendar.CalendarService
   * @throws UnknownDocumentException if the feed is not an event feed.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def getEvents(username: String, visibility: String, projection: String, 
      q: Query): eventsFeed.Feed = {
    val baseUrl = (CalendarService.FEEDS
        + "/" + username
        + "/" + visibility
        + "/" + projection)
    getEvents(q.mkUrl(baseUrl))
  }
  
  /**
   * Add the given event to the user's calendar. Returns the event with possible new
   * fields added by the server. The event should not have the id field set.
   * 
   * @throws IllegalArgumentException if the 'id' field is defined.
   * @throws UnknownDocumentException if the feed is not an event feed.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def addEvent(url: URL, e: eventsFeed.Entry): eventsFeed.Entry = {
    if (e.id.isDefined)
      throw new IllegalArgumentException("New events should have no id.")
    insert(url, e, eventsFeed.entryPickler)
  }
  
  /**
   * Update the given event on the server. Returns the updated event, as returned by
   * the server. The given event should have a link with a 'rel' field set to 'edit'.
   * This method will connect to that link and issue an update request.
   * 
   * @throws IllegalArgumentException if the 'id' field is defined.
   * @throws UnknownDocumentException if the feed is not an event feed.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def updateEvent(e: eventsFeed.Entry): eventsFeed.Entry = {
    e.linkHref("edit") match {
      case Some(url) =>
        updateEvent(new URL(url), e)
      case None =>
        throw new IllegalArgumentException("The given event has no 'edit' link.")
    }
  }

  /**
   * Update the given event on the server. Returns the updated event, as returned by
   * the server.
   * 
   * @throws UnknownDocumentException if the feed is not an event feed.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def updateEvent(url: URL, e: eventsFeed.Entry): eventsFeed.Entry = {
    update(url, e, eventsFeed.entryPickler)
  }
  
  /**
   * Retrieve the access control list for the calender at the given URL. The acl feed URL
   * is found in a link element with rel set to http//schemas.google.com/acl/2007#accessControlList.
   * Only owners of a calendar can retrieve the ACL feed.
   * 
   * @throws UnknownDocumentException if the feed is not an event feed.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def getAccessControlList(url: URL): aclFeed.Feed = {
    query(url, aclFeed.feedPickler)
  }
  
  /**
   * Retrieve the access control list for the given calender. Only owners of a calendar
   * can retrieve the ACL feed. Returns the ACL feed if the calendar has the ACL link,
   * or None.
   * 
   * @throws UnknownDocumentException if the feed is not an event feed.
   * @throws AuthenticationException (one of its subclasses) if the operation fails because
   *         of insufficient rights.
   * @throws IOException if there are connection issues.
   * @throws GDataRequestException if there are HTTP errors.
   */
  def getAccessControlList(cal: calendarsFeed.Entry): Option[aclFeed.Feed] = {
    cal.linkHref(CalendarService.ACL_REL) map (href => getAccessControlList(new URL(href)))
  }
}

object CalendarService {
  final val FEEDS = "http://www.google.com/calendar/feeds"
  
  // visibility
  /** Shows only public events. Does not require authentication. */
  final val PUBLIC = "public"
  
  /** Shows both public and private events. Requires authentication. */
  final val PRIVATE = "private"
  
  // projections
  
  /** Full-fidelity feed; contains all event properties, but comments aren't included inline. */
  final val FULL = "full"
  
  /** Same as full, but without any <gd:who> elements. */
  final val FULL_NOATTENDEES = "full-noattendees"
  
  /** Same as full, but additionally contains inlined comments. */
  final val COMPOSITE = "composite"

  /** Attendees-only feed. Contains minimal event information. */
  final val ATTENDEES_ONLY = "attendees-only"
  
  /** Free/busy feed. Contains minimal event information, but includes <gd:when>. */
  final val FREE_BUSY = "free-busy"
  
  /** Basic Atom feed without any extension elements. */
  final val BASIC = "basic"
  
  /** Rel value for ACL feed links. */
  final val ACL_REL = "http//schemas.google.com/acl/2007#accessControlList"
  final val ACL_RULE = "http//schemas.google.com/acl/2007#accessRule"
  
  /** Rel value for controlled object links. */
  final val CONTROLLED_OBJECT_REL = "http://schemas.google.com/acl/2007#controlledObject"
  
  final val ROLE_NONE = "http://schemas.google.com/gCal/2005#none"
  final val ROLE_READ = "http://schemas.google.com/gCal/2005#read"
  final val ROLE_FREEBUSY = "http://schemas.google.com/gCal/2005#freebusy"
  final val ROLE_EDITOR = "http://schemas.google.com/gCal/2005#editor"
  final val ROLE_OWNER = "http://schemas.google.com/gCal/2005#owner"
  final val ROLE_ROOT = "http://schemas.google.com/gCal/2005#root"
}
