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
 * Refine Entry to the contact kind. It represents contact infromation for an
 * entity, including emails, phone numbers, addresses etc.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html
 */
trait ContactEntries extends AtomEntries {
  type Entry <: ContactEntry
  
  /**
   * A contact entry kind. It represents contact infromation for an entity, including
   * emails, phone numbers, addresses etc.
   * 
   * @author Iulian Dragos
   * @see http://code.google.com/apis/gdata/elements.html
   */
  class ContactEntry extends AtomEntry {
    /** Email addresses. */
    var emails: List[Email] = Nil
    
    /** IM addresses. */
    var instantMessageIds: List[Im] = Nil

    /** Phone and fax numbers. */
    var phoneNumbers: List[PhoneNumber] = Nil
    
    /** Postal addresses. */
    var postalAddresses: List[PostalAddress] = Nil
    
    /** Organizations. */
    var organizations: List[Organization] = Nil
    
    /** Deleted status. */
    var deleted: Boolean = false
    
    /** Convenience constructor for a contact with several emails. */
    def this(title: String, content: String, emails: Email*) {
      this()
      this.title = new Text(title)
      this.content = Some(TextContent(content))
      this.emails = emails.toList
    }
    
    /** Show emails and phone numbers of this contact. */
    override def toString: String = {
      val sb = new StringBuilder
      sb.append(title.content)
      sb.append("\nEmails: ")
      for (e <- emails) sb.append(" ").append(e.address)
      sb.append("\nPhones:")
      for (e <- phoneNumbers) sb.append(" ").append(e.phone)
      sb.toString
    }
    
    /** Fill fields declared by this class, using the given parameters. */
    def fillOwnFields(emails: List[Email], instantMessageIds: List[Im], 
        phoneNumbers: List[PhoneNumber], postalAddresses: List[PostalAddress],
        organizations: List[Organization], deleted: Boolean): this.type = {
      this.emails = emails
      this.instantMessageIds = instantMessageIds
      this.phoneNumbers = phoneNumbers
      this.postalAddresses = postalAddresses
      this.organizations = organizations
      this.deleted = deleted
      this
    }
    
    /** Initialize all known fields from the given entry. */
    def fromContactEntry(ce: ContactEntry): this.type = {
      fromAtomEntry(ce)
      fillOwnFields(ce.emails, ce.instantMessageIds, ce.phoneNumbers, ce.postalAddresses, 
          ce.organizations, ce.deleted)
    }
  }
  
  /** A pickler for contact entry contents. */
  protected def contactEntryContentsPickler: Picklers.Pickler[ContactEntry] = {
    import Picklers._
    
    val ctents = interleaved(atomEntryContentsPickler ~ rep(Email.pickler) ~ rep(Im.pickler)
        ~ rep(PhoneNumber.pickler) ~ rep(PostalAddress.pickler) ~ rep(Organization.pickler)
        ~ marker(elem("deleted", text)(Uris.gdNs)))
    
    wrap (ctents) ({
      case ae ~ emails ~ instantMessageIds ~ phoneNumbers ~ postalAddresses ~ organizations ~ deleted =>
        val ce = new ContactEntry().fromAtomEntry(ae)
        ce.fillOwnFields(emails, instantMessageIds, phoneNumbers, postalAddresses, 
            organizations, deleted)
    }) (fromContactEntry)
  }
  
  private def fromContactEntry(ce: ContactEntry) = {
    (new ~(ce, ce.emails) ~ ce.instantMessageIds ~ ce.phoneNumbers 
         ~ ce.postalAddresses ~ ce.organizations ~ ce.deleted)
  }
}
