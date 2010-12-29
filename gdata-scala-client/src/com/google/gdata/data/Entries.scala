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

import com.google.xml.combinators.{Picklers, ~, HasStore}
import Picklers._

/** 
 * An interface for abstract entries. It is further refined by providing lower
 * bounds for the Entry type. @see AtomEntries.
 *
 * @author Iulian Dragos
 */
trait Entries {
  /** The abstract type for entries. */
  type Entry <: HasStore
  
  /** A pickler for the abstract type Entry. */
  def entryPickler: Pickler[Entry] = elem("entry", makeExtensible(entryContentsPickler))(Uris.atomNs)
  
  /** An abstract pickler for entries. Subclasses need to implement this. */
  protected def entryContentsPickler: Pickler[Entry]
}
