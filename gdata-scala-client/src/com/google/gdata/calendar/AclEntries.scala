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

import com.google.gdata.data.{AtomEntries, kinds, Uris}
import com.google.gdata.data.kinds.{ContactEntries, Where}
import com.google.xml.combinators.{Picklers, ~}

/**
 * Extends atom entries with access control elements, used by calendar.
 * 
 * @author Iulian Dragos
 */
trait AclEntries extends AtomEntries {
  type Entry <: AclEntry
  
  /**
   * An atom entry that has additional elements for describing access control lists.
   * 
   * @see http://code.google.com/apis/calendar/developers_guide_protocol.html#SharingACalendar
   */
  class AclEntry extends AtomEntry {
    /** The scope of this acl entry. */
    var scope: AclScope = _
    
    /** The given role for this scope. */
    var role: String = _
    
    /** 
     * Convenience constructor.
     * 
     * @param tpe Scope type (user, domain or default)
     * @param value (username or domain name)
     * @param role User's role in this access rule.
     */
    def this(tpe: String, value: String, role: String) {
      this()
      this.scope = AclScope(tpe, Some(value))
      this.role = role
      categories = List(data.Category(CalendarService.ACL_RULE, Some(data.kinds.Schemas.KIND), None))
    }
    
    /** Copy contents from the given AclEntry */
    def fromAclEntr(ae: AclEntry) = {
      this.fromAtomEntry(ae)
      fillOwnFields(ae.scope, ae.role)
    }
    
    /** Fill fields defined by AclScope */
    def fillOwnFields(scope: AclScope, role: String): this.type = {
      this.scope = scope
      this.role = role
      this
    }
  }
  
  /** A pickler for access control entries. */
  protected def aclEntryContentsPickler: Picklers.Pickler[AclEntry] = {
    import Picklers._
    
    def fromAclEntry(aclEntry: AclEntry) = 
      new ~(aclEntry, new ~(aclEntry.scope, aclEntry.role))
    
    implicit val aclNs = Uris.gAclNs
    
    wrap (atomEntryContentsPickler 
        ~ interleaved(AclScope.pickler ~ elem("role", attr("value", text)))) ({
    case ae ~ (scope ~ role) =>
      val aclEntry = new AclEntry
      aclEntry.fromAtomEntry(ae)
      aclEntry.fillOwnFields(scope, role)
    }) (fromAclEntry)
  }
}
