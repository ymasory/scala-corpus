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
 * A GData extended property is a name-value pair of Strings.
 */
case class ExtendedProperty(name: String, value: String)

object ExtendedProperty {
  import Picklers._
  
  lazy val pickler: Pickler[ExtendedProperty] = {
    val contents = elem("extendedProperty", attr("name", text) ~ attr("value", text))(Uris.gdNs)
    
    wrap (contents) (ExtendedProperty.apply) (fromExtendedProperty)
  }
  
  private def fromExtendedProperty(ep: ExtendedProperty) =
    new ~(ep.name, ep.value)
}