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

/**
 * Mix in this trait in any container of links. It allows retrieving links based on the
 * 'rel' attribute
 * 
 * @author Iulian Dragos
 * @see AtomEntries, AtomFeeds
 */
trait LinkNavigation {
  def links: List[Link]
  
  /** Return the link element that has the given 'rel' attribute. */
  def link(rel: String): Option[Link] = {
    links.find(_.rel == Some(rel))
  }
  
  /** Return the 'href' field of the link that matches the required 'rel' attribute. */
  def linkHref(rel: String): Option[String] =
    link(rel) map (_.href)
  
  /** Return the 'href' part of a link with relation 'edit', if any. */
  def editLink: Option[String] = 
    linkHref("edit")
  
  /** Return the 'href' part of a link with relation 'post', if any. */
  def postLink: Option[String] =
    linkHref("post")
}
