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


package com.google.gdata.data.kinds

import com.google.xml.combinators.{Picklers, ~}
import com.google.gdata.data.Uris

/**
 * Represents a logically nested entry. For example, a <gd:who> representing a contact
 * might have a nested entry from a contact feed.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdEntryLink
 */
class EntryLink[Entry] {
  /**
   * Specifies the entry URI. If the nested entry is embedded and not linked, 
   * this attribute may be omitted. 
   */
  var href: Option[String] = None

  /** Specifies whether the contained entry is read-only. The default value is "false". */
  var readOnly: Boolean = false
  
  /** Specifies the link relation. */
  var rel: Option[String] = None
  
  /** Contents of the entry. */
  var entry: Option[Entry] = None
  
  def fillOwnFields(href: Option[String], readOnly: Boolean, 
                    rel: Option[String], entry: Option[Entry]) = {
    this.href = href
    this.readOnly = readOnly
    this.rel = rel
    this.entry = entry
    this
  }
}

object EntryLink {
  import Picklers._
  
  def pickler[Entry](pe: Pickler[Entry]) = wrap (elem("entryLink",
      opt(attr("href", text)) ~ default(attr("readOnly", boolVal), false) ~ opt(attr("rel", text))
      ~ opt(pe))(Uris.gdNs)) (new EntryLink[Entry]().fillOwnFields _) (fromEntryLink)
  
  private def fromEntryLink[Entry](el: EntryLink[Entry]) = 
    new ~(el.href, el.readOnly) ~ el.rel ~ el.entry
}
