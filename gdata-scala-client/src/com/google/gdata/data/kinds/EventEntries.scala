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
import com.google.gdata.data.util.DateTime
import com.google.gdata.data.{AtomFeeds, AtomEntries}

/**
 * An event kind. It extends Atom entries with elements for describing a calendar
 * event: attendees, location, recurrence, comments, etc.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdEventKind
 */
trait EventEntries extends AtomEntries {
  type Entry <: EventEntry
  
  val commentsFeed: AtomFeeds
  val contactEntries: ContactEntries
  
  /**
   * An entry that describes a calendar event.
   * 
   * @see http://code.google.com/apis/gdata/elements.html#gdEventKind
   */
  class EventEntry extends AtomEntry {
    /** Comments feed. */
    var comments: Option[Comments[commentsFeed.Feed]] = None
    
    /** Event status. */
    var eventStatus: Option[String] = None
    
    /** Recurrence rule, in iCalendar format (RFC 2445). */
    var recurrence: Option[String] = None
    
    /** Whether this event marks time as busy or not (see RFC 2445). */
    var transparency: Option[String] = None
    
    /** Event visibility, like public or private. */
    var visibility: Option[String] = None
    
    /** Event time. */
    var when: List[When] = Nil
    
    /** Locations where this event takes place. */
    var locations: List[Where[contactEntries.Entry]] = Nil
    
    /** Persons associated with this event. */
    var participants: List[Who[contactEntries.Entry]] = Nil
    
    /** Is this entry supposed to be 'quick add' processed? */
    var quickAdd: Boolean = false
    
    /** Extended properties. */
    var extendedProperties: List[ExtendedProperty] = Nil
    
    /** Reccurence exceptions for this event. */
    var exceptions: List[RecurrenceException[Entry]] = Nil
    
    /** Convenience method for creating a one-shot event entry. */
    def this(title: String, content: String, start: DateTime, end: DateTime) {
      this()
      this.title = new Text(title)
      this.content = Some(TextContent(content))
      this.when = new When(start, end) :: Nil
    }
    
    /**
     * Convenience constructor for quick add events. The given string is parsed by the
     * server and fields like location, time and title are automatically filled in.
     */
    def this(content: String) {
      this()
      this.content = Some(TextContent(content))
      this.quickAdd = true
    }
    
    /** Copy given parameters to the corresponding fields of this entry. */
    def fillOwnFields(comments: Option[Comments[commentsFeed.Feed]], eventStatus: Option[String],
        recurrence: Option[String], transparency: Option[String], visibility: Option[String],
        when: List[When], locations: List[Where[contactEntries.Entry]],
        participants: List[Who[contactEntries.Entry]], quickAdd: Boolean,
        extendedProperties: List[ExtendedProperty], 
        exceptions: List[RecurrenceException[Entry]]): this.type = {
      this.comments = comments
      this.eventStatus = eventStatus
      this.recurrence = recurrence
      this.transparency = transparency
      this.visibility = visibility
      this.when = when
      this.locations = locations
      this.participants = participants
      this.quickAdd = quickAdd
      this.extendedProperties = extendedProperties
      this.exceptions = exceptions
      this
    }
    
    /** Fill in all fields from another EventEntry */
    def fromEventEntry(ev: EventEntry): this.type = {
      fromAtomEntry(ev)
      fillOwnFields(ev.comments, ev.eventStatus, ev.recurrence, ev.transparency, ev.visibility, 
          ev.when, ev.locations, ev.participants, ev.quickAdd, ev.extendedProperties, 
          ev.exceptions)
    }
  }
  
  /** A pickler for event entries. */
  lazy val eventEntryContentsPickler: Picklers.Pickler[EventEntry] = {
    import Picklers._
    
    val contents = interleaved(atomEntryContentsPickler 
        ~ opt(Comments.pickler(commentsFeed.feedPickler))
        ~ opt(Enum.pickler("eventStatus"))
        ~ opt(elem("recurrence", text)(Uris.gdNs))
        ~ opt(Enum.pickler("transparency"))
        ~ opt(Enum.pickler("visibility"))
        ~ rep(When.pickler)
        ~ rep(Where.pickler(contactEntries.entryPickler))
        ~ rep(Who.pickler(contactEntries.entryPickler))
        ~ default(elem("quickAdd", attr("value",boolVal))(Uris.gCalNs), false)
        ~ rep(ExtendedProperty.pickler)
        ~ rep(RecurrenceException.pickler(entryPickler)))
                               
    wrap (contents) {
    case ae ~ comments ~ eventStatus ~ recurrence ~ transparency 
        ~ visibility ~ when ~ locations ~ participants ~ quickAdd ~ extendedProperties
        ~ exceptions =>
      val ce = new EventEntry().fromAtomEntry(ae)
      ce.fillOwnFields(comments, eventStatus, recurrence, transparency,
          visibility, when, locations, participants, quickAdd, extendedProperties, exceptions)
    } (fromEventEntry)
  }
  
  private def fromEventEntry(ce: EventEntry) = 
    (new ~(ce, ce.comments) ~ ce.eventStatus ~ ce.recurrence ~ ce.transparency
        ~ ce.visibility ~ ce.when ~ ce.locations ~ ce.participants ~ ce.quickAdd
        ~ ce.extendedProperties ~ ce.exceptions)
}

/**
 * Hold together enumeration values for event entries.
 */
object EventEntries {
  // event status values
  final val CANCELED = "http://schemas.google.com/g/2005#event.canceled"
  final val CONFIRMED = "http://schemas.google.com/g/2005#event.confirmed"
  final val TENTATIVE = "http://schemas.google.com/g/2005#event.tentative"
  
  // visibility values
  final val CONFIDENTIAL = "http://schemas.google.com/g/2005#event.confidential"
  final val DEFAULT = "http://schemas.google.com/g/2005#event.default"
  final val PRIVATE = "http://schemas.google.com/g/2005#event.private"
  final val PUBLIC = "http://schemas.google.com/g/2005#event.public"
  
  // transparency values
  final val OPAQUE = "http://schemas.google.com/g/2005#event.opaque"
  final val TRANSPARENT = "http://schemas.google.com/g/2005#event.transparent"
  
  // attendee relations
  final val ATTENDEE = "http://schemas.google.com/g/2005#event.attendee"
  final val ORGANIZER = "http://schemas.google.com/g/2005#event.organizer"
  final val PERFORMER = "http://schemas.google.com/g/2005#event.performer"
  final val SPEAKER = "http://schemas.google.com/g/2005#event.speaker"
  
  // attendee types
  final val OPTIONAL = "http://schemas.google.com/g/2005#event.optional"
  final val REQUIRED = "http://schemas.google.com/g/2005#event.required"
  
  // attendee status
  final val ACCEPTED = "http://schemas.google.com/g/2005#event.accepted"
  final val DECLINED = "http://schemas.google.com/g/2005#event.declined"
  final val INVITED = "http://schemas.google.com/g/2005#event.invited"
//  TENTATIVE, the same as the event status, is allowed for attendee status as well
}
