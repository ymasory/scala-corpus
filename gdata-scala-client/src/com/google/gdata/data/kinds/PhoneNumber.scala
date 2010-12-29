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
 * A phone number as exposed by GData
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdPhoneNumber
 */
case class PhoneNumber(
    /** A string value used to identify this phone number. */
    var label: Option[String],
    
    /** The type of this phone number. */
    var rel: Option[String],
    
    /** A phone URI for programmatic access. See RFC 3966. */
    var uri: Option[String],
    
    /** At most one phone number is the primary number. */
    var primary: Boolean,
    
    /**  
     * Human-readable phone number; may be in any telephone number format. 
     * Leading and trailing whitespace is insignificant.
     */
    var phone: String) {
  /** Convenience constructor for a simple, non-primary phone number. */
  def this(phone: String, rel: String) {
    this(None, Some(rel), None, false, phone)
  }
}

object PhoneNumber {
  import Picklers._
 
  def pickler = (wrap (elem("phoneNumber", opt(attr("label", text))
      ~ opt(attr("rel", text)) ~ opt(attr("uri", text))
      ~ default(attr("primary", boolVal), false) ~ text)(Uris.gdNs))
      (PhoneNumber.apply) (fromPhoneNumber))
  
  private def fromPhoneNumber(pn: PhoneNumber) =
    new ~(pn.label, pn.rel) ~ pn.uri ~ pn.primary ~ pn.phone
}
