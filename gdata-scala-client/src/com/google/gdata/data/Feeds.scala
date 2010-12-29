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


package com.google.gdata.data;

import com.google.xml.combinators.{Picklers, ~, HasStore}
import com.google.gdata.data.util.DateTime

import Picklers._
import Atom._

/**
 * A module for Feeds. Use this by mixing it in a class that has an Entries implementation.
 *
 * This module provides a feed and a feed pickler.
 * This module requires an entry type and an entry pickler.
 *
 * @author Iulian Dragos
 */
trait Feeds { this: Feeds with Entries =>
  type Feed <: Seq[Entry] with HasStore
  
  /** A pickler for feeds. */
  def feedPickler: Pickler[Feed] = elem("feed", makeExtensible(feedContentsPickler))(Uris.atomNs)
  
  /** An abstract pickler for feed contents. Subclasses need to implement this method. */
  protected def feedContentsPickler: Pickler[Feed]
}


