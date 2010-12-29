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
 * An organization, typically associated with a contact.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdOrganization
 */
case class Organization(
    /** A string value used to identify this address. */
    var label: Option[String],
    
    /** A programmatic value that identifies the type of postal address. */
    var rel: Option[String],
    
    /** At most one phone number is the primary number. */
    var primary: Boolean,
    
    /** The organization name. */
    var name: Option[String],
    
    /** The title of a person within the organization. */
    var title: Option[String])

object Organization {
  import Picklers._
 
  def pickler = {
    implicit val ns = Uris.gdNs
  
    (wrap (elem("organization", opt(attr("label", text))
        ~ opt(attr("rel", text)) ~ default(attr("primary", boolVal), false)
        ~ opt(elem("orgName", text)) ~ opt(elem("orgTitle", text)))) 
        (Organization.apply) (fromOrganization))
  }
  
  private def fromOrganization(org: Organization) =
    new ~(org.label, org.rel) ~ org.primary ~ org.name ~ org.title
}

