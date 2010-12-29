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
 * A person associated to the containing entity. The relation is specified by the 'rel'
 * field.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdWho
 */
class Who[ContactEntry <: ContactEntries#ContactEntry] {
  /** Email address. This property is typically used when entryLink is not specified. */
  var email: Option[String] = None
  
  /** The relationship between the containing entity and the contained person. */
  var rel: Option[String] = None
  
  /** A simple string value that can be used as a representation of this person. */
  var valueString: Option[String] = None
  
  /** The attendee status. */
  var attendeeStatus: Option[String] = None
  
  /** Type of attendee. */
  var attendeeType: Option[String] = None
  
  /** Entry representing contact details.*/
  var entryLink: Option[EntryLink[ContactEntry]] = None
  
  /**
   * Convenience constructor for a person with an email and a string representation 
   * of its relation. 
   */
  def this(email: String, valueString: String) {
    this()
    this.email = Some(email)
    this.valueString = Some(valueString)
  }
}

object Who {
  import Picklers._
  implicit val ns = Uris.gdNs
  
  def pickler[CEntry <: ContactEntries#ContactEntry](p: Pickler[CEntry])
      : Pickler[Who[CEntry]] = {
    val contents = elem("who", 
          opt(attr("email", text))
        ~ opt(attr("rel", text))
        ~ opt(attr("valueString", text))
        ~ opt(Enum.pickler("attendeeStatus"))
        ~ opt(Enum.pickler("attendeeType"))
        ~ opt(EntryLink.pickler(p)))
    
    wrap (contents) (toWho[CEntry]) (fromWho[CEntry])
  }

  /** Create a Who instance based on the given parameters. */
  private def toWho[CEntry <: ContactEntries#ContactEntry](email: Option[String],
      rel: Option[String], valueString: Option[String],
      attendeeStatus: Option[String], attendeeType: Option[String], 
      entryLink: Option[EntryLink[CEntry]]) = {
    val who = new Who[CEntry]
    who.email = email
    who.rel = rel
    who.valueString = valueString
    who.attendeeStatus = attendeeStatus
    who.attendeeType = attendeeType
    who.entryLink = entryLink
    who
  }
  
  private def fromWho[CEntry <: ContactEntries#ContactEntry](who: Who[CEntry]) =
    (new ~(who.email, who.rel) ~ who.valueString ~ who.attendeeStatus
        ~ who.attendeeType ~ who.entryLink)
}
