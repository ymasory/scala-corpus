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

/**
 * An event that's an exception to a recurring event.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdRecurrenceException
 */
class RecurrenceException[EventEntry <: EventEntries#EventEntry] {
  /** Is the exception specialized? */
  var specialized: Boolean = false
  
  /** Event entry detailing the exception. */
  var entryLink: EntryLink[EventEntry] = _
  
  /** The original recurring event that this instance is an exception to. */
  var originalEvent: OriginalEvent = _
}

object RecurrenceException {
  import Picklers._
  
  /** A pickler for RecurrenceException. */
  def pickler[EventEntry <: EventEntries#EventEntry](pe: Pickler[EventEntry])
      : Pickler[RecurrenceException[EventEntry]] = {
    val contents = elem("recurrenceException", 
        attr("specialized", boolVal)
      ~ EntryLink.pickler(pe)
      ~ OriginalEvent.pickler)(Uris.gdNs)
    
    wrap (contents) ({
      case specialized ~ entryLink ~ originalEvent =>
        val re = new RecurrenceException[EventEntry]
        re.specialized = specialized
        re.entryLink = entryLink
        re.originalEvent = originalEvent
        re
    }) (fromRecurrenceException[EventEntry])
  }
      
  private def fromRecurrenceException[EventEntry <: EventEntries#EventEntry]
        (re: RecurrenceException[EventEntry]) = {
    new ~(re.specialized, re.entryLink) ~ re.originalEvent
  }
}
