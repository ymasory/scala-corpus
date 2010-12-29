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


package com.google.gdata.data.media

import com.google.xml.combinators.~
import com.google.xml.combinators.Picklers._
import com.google.gdata.data.Uris.mediaNs  

/**
 * A simple text element. It can be used for media:title and
 * media:description elements.  @see http://search.yahoo.com/mrss
 *
 * @author Iulian Dragos
 */
case class SimpleText(tpe: String, value: String)

object SimpleText {
  /** Return a pickle for the given element name. */
  def pickler(elemName: String): Pickler[SimpleText] =
    (wrap (elem(elemName, default(attr("type", text), "plain") ~ text)(mediaNs)) 
        (SimpleText.apply)
        (t => new ~(t.tpe, t.value)))
}