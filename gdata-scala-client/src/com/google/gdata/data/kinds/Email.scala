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
 * An email address as exposed by GData.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdEmail
 */
case class Email(
  /** Email address */
  var address: String,
  
  /** A simple string value used to name this email address (like 'Work', 'Home'). */
  var label: Option[String],
  
  /** A programmatic value that identifies the type of email. */
  var rel: Option[String],
  
  /** At most one email may be primary. Default value is "false". */
  var primary: Boolean) {
  
  /** Convenience constructor. */
  def this(address: String, rel: String) {
    this(address, None, Some(rel), false)
  }
}

object Email {
  import Picklers._
  
  def pickler = (wrap (elem("email", attr("address", text) ~ opt(attr("label", text))
      ~ opt(attr("rel", text))
      ~ default(attr("primary", boolVal), false))(Uris.gdNs)) 
      (Email.apply) (fromEmail))
  
  private def fromEmail(e: Email) = 
    new ~(e.address, e.label) ~ e.rel ~ e.primary
}
