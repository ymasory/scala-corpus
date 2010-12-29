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
 * A media:text element, as defined by Media RSS
 * 
 * @see http://search.yahoo.com/mrss
 * @author Iulian Dragos 
 */
case class Text(tpe: String, value: String, lang: Option[String], 
                start: Option[NormalPlayTime], end: Option[NormalPlayTime])

object Text {
  
  val pickler: Pickler[Text] = 
    (wrap (elem("text", 
        default(attr("type", text), "plain")
      ~ text
      ~ opt(attr("lang", text))
      ~ opt(attr("start", NormalPlayTime.pickler))
      ~ opt(attr("end", NormalPlayTime.pickler)))(mediaNs))
      (Text.apply)
      (fromText))
  
  private def fromText(t: Text) = new ~(t.tpe, t.value) ~ t.lang ~ t.start ~ t.end
}
