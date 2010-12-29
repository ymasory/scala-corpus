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
import com.google.gdata.data.util.NormalPlayTime
import com.google.gdata.data.Uris.mediaNs

/**
 * A media:credit element, as defined by Media RSS
 * 
 * @see http://search.yahoo.com/mrss
 * @author Iulian Dragos 
 */
case class Credit(scheme: String, role: Option[String], value: String)

object Credit {
  /** Default credit scheme is European Broadcasting Union Role Codes. */
  val DEFAULT_SCHEME = "urn:ebu"
  
  val pickler: Pickler[Credit] = 
    (wrap (elem("credit", default(attr("scheme", text), DEFAULT_SCHEME) ~ opt(attr("role", text)) ~ text)(mediaNs))
       (Credit.apply)
       (fromCredit))
  
  private def fromCredit(c: Credit) = new ~(c.scheme, c.role) ~ c.value
}