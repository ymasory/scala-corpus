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
 * A postal address, as represented by GData.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdPostalAddress
 */
case class PostalAddress(
    /** A string value used to identify this address. */
    var label: Option[String],
    
    /** A programmatic value that identifies the type of postal address. */
    var rel: Option[String],
    
    /** At most one phone number is the primary number. */
    var primary: Boolean,
    
    /**  
     * Human-readable address; Leading and trailing whitespace is insignificant, 
     * newlines are signifficant.
     */
    var address: String) {
  
  /** Convenience constructor for a simple, non-primary postal address. */
  def this(address: String, rel: String) {
    this(None, Some(rel), false, address)
  }
}

object PostalAddress {
  import Picklers._
 
  def pickler = (wrap (elem("postalAddress", opt(attr("label", text))
      ~ opt(attr("rel", text)) ~ default(attr("primary", boolVal), false)
      ~ text)(Uris.gdNs)) (PostalAddress.apply) (fromPostalAddress))
  
  private def fromPostalAddress(pa: PostalAddress) = 
    new ~(pa.label, pa.rel) ~ pa.primary ~ pa.address
}
