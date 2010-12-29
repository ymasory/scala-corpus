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
package media;

import com.google.xml.combinators.~
import com.google.xml.combinators.Picklers._
import com.google.gdata.data.util.NormalPlayTime

import scala.xml.{NamespaceBinding, TopScope}

/**
 * A media:hash element, as defined by Media RSS
 * 
 * @see http://search.yahoo.com/mrss
 * @author Iulian Dragos 
 */
case class Hash(algo: String, value: String)

object Hash {
  val pickler: Pickler[Hash] = 
    (wrap (elem("hash", default(attr("algo", text), "md5") ~ text) (Uris.mediaNs))
       (Hash.apply)
       (fromHash))
  
  def fromHash(h: Hash) = new ~(h.algo, h.value)
}
