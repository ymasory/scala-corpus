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

import com.google.xml.combinators.{Picklers, ~, HasStore}

import scala.xml.{NamespaceBinding, TopScope}

import Picklers._
 
/**
 * An Atom link construct, as defined by the Atom spec.
 * 
 * @see http://atomenabled.org/developers/syndication/atom-format-spec.php
 */
case class Link(href: String, 
    rel: Option[String],
    tpe: Option[String],
    hrefLang: Option[String],
    title: Option[String],
    length: Option[String]) extends HasStore
    
    
object Link {
  implicit val nsAtom = Uris.atomNs
  
  private lazy val contentsPickler: Pickler[Link] = wrap(attr("href", text) 
        ~ opt(attr("rel", text))
        ~ opt(attr("type", text))
        ~ opt(attr("hrefLang", text))
        ~ opt(attr("title", text))
        ~ opt(attr("length", text))) (Link.apply) (toPair)
  
  lazy val pickler: Pickler[Link] = elem("link", makeExtensible(contentsPickler))
        
  private def toPair(v: Link) = 
    new ~(v.href, v.rel) ~ v.tpe ~ v.hrefLang ~ v.title ~ v.length
}
