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
 * A place (such as an event location) associated with the containing entity. 
 * The type of the association is determined by the rel attribute; the details
 * of the location are contained in an embedded or linked-to Contact entry.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdWhere
 */
class Where[CEntry <: ContactEntries#ContactEntry] {
  /** 
   * The type of contact entries has to be a subtype of the Contact kind in a module that
   * mixes in ContactEntries.
   */
  type ContactEntry = CEntry 
  
  /** Specifies a user-readable label to distinguish this location from other locations. */
  var label: Option[String] = None
  
  /** Specifies the relationship between the containing entity and the contained location. */
  var rel: Option[String] = None
  
  /** A simple string value that can be used as a representation of this location. */
  var valueString: Option[String] = None
  
  /** Entry representing location details. This entry should implement the Contact kind. */
  var entry: Option[EntryLink[ContactEntry]] = None
  
  /** Convenience method for creating locations. */
  def this(valueString: String, rel: String) {
    this()
    this.valueString = Some(valueString)
    this.rel = Some(rel)
  }
  
  def fillOwnFields(label: Option[String], rel: Option[String], valueString: Option[String],
      entry: Option[EntryLink[ContactEntry]]) = {
    this.label = label
    this.rel = rel
    this.valueString = valueString
    this.entry = entry
    this
  }
}

object Where {
  import Picklers._
  
  def pickler[CEntry <: ContactEntries#ContactEntry](p: Pickler[CEntry]): Pickler[Where[CEntry]] = {
    (wrap (elem("where", opt(attr("label", text)) ~ opt(attr("rel", text))
         ~ opt(attr("valueString", text))
         ~ opt(EntryLink.pickler(p)))(Uris.gdNs)) {
       case label ~ rel ~ valueString ~ entry =>
         new Where[CEntry].fillOwnFields(label, rel, valueString, entry)
       } (fromWhere))
  }
    
  private def fromWhere[CEntry <: ContactEntries#ContactEntry](where: Where[CEntry])
        : Option[String] ~ Option[String] ~ Option[String] ~ Option[EntryLink[CEntry]]= 
    new ~(where.label, where.rel) ~ where.valueString ~ where.entry
}
