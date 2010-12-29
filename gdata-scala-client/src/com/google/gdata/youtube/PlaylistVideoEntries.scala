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


package com.google.gdata.youtube

import com.google.gdata.data.media.MediaRss
import com.google.gdata.data.Uris
import com.google.xml.combinators.{Picklers, ~}

/**
 * This trait refines video entries to add position and an optional description.
 * 
 * @author Iulian Dragos
 */
trait PlaylistVideoEntries extends VideoEntries { this: PlaylistVideoEntries with MediaRss =>
  type Entry <: PlaylistVideoEntry
  
  /**
   * A video entry in a playlist. It adds an optional description and a position.
   */
  class PlaylistVideoEntry extends VideoEntry {
    var description: Option[String] = None
    
    /** Video position in the playlist. */
    var position: Int = 0
    
    def fromPlaylistVideoEntry(pve: PlaylistVideoEntry) {
      fromVideoEntry(pve)
      fillOwnFields(pve.description, pve.position)
    }
    
    def fillOwnFields(description: Option[String], position: Int): this.type = {
      this.description = description
      this.position = position
      this
    }
  }
  
  protected def playlistVideoEntryContentsPickler = {
    import Picklers._
    
    val contents = interleaved(
      videoEntryContentsPickler ~ opt(elem("description", text)(Uris.ytNs))
          ~ elem("position", intVal)(Uris.ytNs))
    
    def fromPlaylistVideoEntry(pve: PlaylistVideoEntry) =
      new ~(pve, pve.description) ~ pve. position
    
    wrap (contents) {
      case ve ~ description ~ position =>
        val pve = new PlaylistVideoEntry
        pve.fromVideoEntry(ve)
        pve.fillOwnFields(description, position)
    } (fromPlaylistVideoEntry)
  }
}
