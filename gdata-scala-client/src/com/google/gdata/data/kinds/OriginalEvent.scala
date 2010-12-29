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
 * Identifies a particular instance in a recurrent event. Equivalent to Recurrence ID in
 * RFC 2445.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdOriginalEvent
 */
case class OriginalEvent(
    /** The ID of the original event. */
    var id: String,
  
    /** The event feed URL for the original event. */
    var href: String,
  
    /** The original start time of the instance that has become an exception. */
    var when: When)

object OriginalEvent {
  import Picklers._
  
  /** A pickler for original events. */
  lazy val pickler: Pickler[OriginalEvent] = {
    val contents = elem("originalEvent", 
        attr("id", text)
      ~ attr("href", text)
      ~ When.pickler)(Uris.gdNs)
    
    wrap (contents) (OriginalEvent.apply) (fromOriginalEvent)
  }
  
  private def fromOriginalEvent(oe: OriginalEvent) =
    new ~(oe.id, oe.href) ~ oe.when
}
