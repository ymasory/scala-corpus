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
 * An instant messaging address, as exposed by GData.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdIm
 */
case class Im(var address: String,
    var label: Option[String],
    var rel: Option[String],
    var protocol: Option[String],
    var primary: Boolean) {
  
  /** Convenience constructor for an instant messaging address. */
  def this(address: String, rel: String, protocol: String) {
    this(address, None, Some(rel), Some(protocol), false)
  }
}

/** Defines a pickler for Im. */
object Im {
  import Picklers._
  
  def pickler = (wrap (elem("im", attr("address", text) ~ opt(attr("label", text))
      ~ opt(attr("rel", text)) ~ opt(attr("protocol", text))
      ~ default(attr("primary", boolVal), false))(Uris.gdNs)) 
      (Im.apply) (fromIm))
  
  private def fromIm(im: Im) =
    new ~(im.address, im.label) ~ im.rel ~ im.protocol ~ im.primary
}
