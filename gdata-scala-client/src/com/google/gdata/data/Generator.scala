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

import com.google.xml.combinators.{Picklers, ~}

import Picklers._
 
/**
 * An Atom generator construct, as defined by the Atom spec.
 * 
 * @see http://atomenabled.org/developers/syndication/atom-format-spec.php
 */
case class Generator(name: String, uri: Option[String], version: Option[String])

object Generator {
  val pickler: Pickler[Generator] =
    (wrap (elem("generator", text ~ opt(attr("uri", text)) ~ opt(attr("version", text)))(Uris.atomNs))
          (Generator.apply) (toPairs))

  private def toPairs(v: Generator) = new ~(v.name, v.uri) ~ v.version
}
