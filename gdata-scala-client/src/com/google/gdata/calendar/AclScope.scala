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


package com.google.gdata.calendar

import com.google.gdata.data.Uris
import com.google.xml.combinators.{Picklers, ~}

/**
 * An access control list scope. Type can be 'user', 'domain' or 'default'. Value can be
 * an email address or a domain name.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/calendar/reference.html#gacl_reference
 */
case class AclScope (tpe: String, value: Option[String])

object AclScope {
  import Picklers._
  
  def pickler: Pickler[AclScope] = 
    wrap(elem("scope", 
        attr("type", text)
      ~ opt(attr("value", text)))(Uris.gAclNs)) (AclScope.apply) (fromAclScope)
  
  private def fromAclScope(s: AclScope) =
    new ~(s.tpe, s.value)
}