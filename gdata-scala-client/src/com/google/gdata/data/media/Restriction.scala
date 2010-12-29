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


package com.google.gdata.data.media;

import com.google.xml.combinators.~
import com.google.xml.combinators.Picklers._
import com.google.gdata.data.Uris.mediaNs

/**
 * A media:restriction element, as defined by Media RSS
 * 
 * @see http://search.yahoo.com/mrss
 * @author Iulian Dragos 
 */
case class Restriction(relationship: String, tpe: Option[String], value: List[String])

object Restriction {
  
  val pickler: Pickler[Restriction] =
    (wrap (elem("restriction", attr("relationship", text) ~ opt(attr("type", text)) ~ list(' ', text))(mediaNs))
        (Restriction.apply)
        (fromRestriction))
  
  private def fromRestriction(r: Restriction) = new ~(r.relationship, r.tpe) ~ r.value
}