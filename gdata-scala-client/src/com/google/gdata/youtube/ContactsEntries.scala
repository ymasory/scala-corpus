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


package com.google.gdata.youtube

import com.google.xml.combinators.{Picklers, ~}
import com.google.gdata.data.{AtomEntries, Uris, Text}

/**
 * Youtube user contact entries.
 */
trait ContactsEntries extends AtomEntries {
  type Entry <: ContactEntry

  /**
   * A user contact entry.
   */
  class ContactEntry extends AtomEntry {
    var username: String = ""
    var status: String = ""
    
    def fromContactEntry(ce: ContactEntry) {
      fromAtomEntry(ce)
      fillOwnFields(ce.username, ce.status)
    }
    
    /** 
     * Convenience constructor. It creates an entry with the given username and status.
     * The entry atom title is set to the username.
     */
    def this(username: String, status: String) {
      this()
      this.title = new Text(username)
      this.username = username
      this.status = status
    }
    
    def fillOwnFields(username: String, status: String): this.type = {
      this.username = username
      this.status = status
      this
    }
    
    override def toString = {
      super.toString + "\nusername: " + username + "\nstatus: " + status
    }
  }
  
  /** A pickler for contact entries. */
  protected def contactEntryContentsPickler: Picklers.Pickler[ContactEntry] = {
    import Picklers._

    def fromContactEntry(ce: ContactEntry) = 
      new ~(ce, ce.username) ~ ce.status
    
    val contents =
      interleaved(atomEntryContentsPickler ~ elem("username", text)(Uris.ytNs)
                  ~ elem("status", text)(Uris.ytNs))
    wrap (contents) {
      case ae ~ username ~ status =>
        val ce = new ContactEntry
        ce.fromAtomEntry(ae)
        ce.fillOwnFields(username, status)
    } (fromContactEntry)
  }
}
