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


package com.google.gdata.calendar

import com.google.gdata.data.{AtomEntries, kinds, Uris}
import com.google.gdata.data.kinds.{ContactEntries, Where}
import com.google.xml.combinators.{Picklers, ~}

/**
 * Entries in a calendar's feed. Each entry describes a calendar.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/calendar/developers_guide_protocol.html
 */
trait CalendarEntries extends AtomEntries {
  type Entry <: CalendarEntry
  
  /** 
   * Contact entries are required for correctly parsing calendar locations (<where> elements)
   * Classes that mix-in this trait should provide an instance of the desired ContactEntry
   * module.
   * 
   * @see com.google.gdata.data.kinds.StdContactEntries
   */
  val contactEntries: ContactEntries
  
  class CalendarEntry extends AtomEntry {
    /** The level of access the current user has to the calendar. */
    var accessLevel: String = "none"
    
    /** The color used to highlight a calendar in the user's browser. */
    var color: String = ""
    
    /** Indicates whether a calendar is visible or not. */
    var hidden: Boolean = false
    
    /** Indicates whether calendar is selected. */
    var selected: Boolean = true
    
    /** Calendar's timezone. */
    var timeZone: String = ""

    /** Calendar's locations. */
    var locations: List[Where[contactEntries.Entry]] = Nil
    
    /** Fill own fields with the given parameters. */
    def fillOwnFields(accessLevel: String, color: String, hidden: Boolean, 
        selected: Boolean, timeZone: String, 
        locations: List[Where[contactEntries.Entry]]): this.type = {
      this.accessLevel = accessLevel
      this.color = color
      this.hidden = hidden
      this.selected = selected
      this.timeZone = timeZone
      this.locations = locations
      this
    }
    
    /** 
     * Fill own fields from another instance of a calendar entry. Useful for subclasses that
     * might want to reuse existing data.
     */
    def fromCalendarEntry(ce: CalendarEntry): this.type = {
      fromAtomEntry(ce)
      fillOwnFields(ce.accessLevel, ce.color, ce.hidden, ce.selected, ce.timeZone, ce.locations)
    }
  }
  
  /** A pickler for CalendarEntry contents. */
  protected def calendarEntryContentsPickler: Picklers.Pickler[CalendarEntry] = {
    import Picklers._
    implicit val ns = Uris.gCalNs
    
    /** A simple gCal element with one attribute called 'value'. */
    def gCalElement[A](name: String, pa: Pickler[A]): Pickler[A] = 
      elem(name, attr("value", pa))
    
    // Contents of a calendar entry
    val ctents = interleaved(atomEntryContentsPickler ~ gCalElement("accesslevel", text)
        ~ gCalElement("color", text) ~ gCalElement("hidden", boolVal)
        ~ gCalElement("selected", boolVal) ~ gCalElement("timezone", text)
        ~ rep(Where.pickler(contactEntries.entryPickler)))
    
    wrap (ctents) ({
      case ae ~ accessLevel ~ color ~ hidden ~ selected ~ timeZone ~ locations =>
        val ce = new CalendarEntry()
        ce.fromAtomEntry(ae)
        ce.fillOwnFields(accessLevel, color, hidden, selected, timeZone, locations)
        ce
    }) (fromCalendarEntry)
  }
  
  private def fromCalendarEntry(ce: CalendarEntry) =
    new ~(ce, ce.accessLevel) ~ ce.color ~ ce.hidden ~ ce.selected ~ ce.timeZone ~ ce.locations
}
