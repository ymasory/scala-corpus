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
 * A media:thumbnail element, as defined by the Media RSS spec.
 * 
 * @see http://search.yahoo.com/mrss
 * @author Iulian Dragos 
 */
case class Thumbnail(url: String, width: Option[Int], height: Option[Int],
                    time: Option[NormalPlayTime])

object Thumbnail {

  /** A pickler for thumbnail elements. */
  val pickler: Pickler[Thumbnail] = 
    (wrap (elem("thumbnail", 
                attr("url", text)
              ~ opt(attr("width", intVal))
              ~ opt(attr("height", intVal))
              ~ opt(attr("time", NormalPlayTime.pickler)))(Uris.mediaNs))
           (Thumbnail.apply)
           (fromThumbnail))
  
  private def fromThumbnail(v: Thumbnail) =
    new ~(v.url, v.width) ~ v.height ~ v.time
}
