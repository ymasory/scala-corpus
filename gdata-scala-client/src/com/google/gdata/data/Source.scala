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


package com.google.gdata.data;

import com.google.xml.combinators.{Picklers, ~}
import com.google.gdata.data.util.DateTime

import Picklers._
import Atom._

/** 
 * An Atom source element. It is used when entries are copied over from another feed
 * to keep around metadata bout the source feed.
 * 
 * @see http://atomenabled.org/developers/syndication/atom-format-spec.php#element.source
 */
class Source {
  /** The authors of the originating feed. */
  var authors: List[Person] = Nil
  
  /** Categories associated with the originating feed. */
  var categories: List[Category] = Nil
  
  /** Contributors associated to the originating feed. */  
  var contributors: List[Person] = Nil
  
  /** User agent used to generate the originating feed. */
  var generator: Option[Generator] = None
  
  /** IRI for an icon for the originating feed. */
  var icon: Option[String] = None
  
  /** Permanent, unique id of the originating feed. */
  var id: Option[String] = None
  
  /** References to web resources. */
  var links: List[Link] = Nil
  
  /** IRI for a visual identification of the originating feed. */
  var logo: Option[String] = None
  
  /** Rights held over the originating feed. */
  var rights: Option[String] = None
    
  /** A subtitle of the originating feed. */
  var subtitle: Option[Text] = None
    
  /** The originating feed's title. */
  var title: Option[Text] = None
    
  /** The most recent instant in time the originating feed has been modified. */
  var updated: Option[DateTime] = None

  /** Convenience method for creating a source element from an atom Feed. */
  def this(feed: AtomFeeds#Feed) {
    this()
    this.authors = feed.authors
    this.categories = feed.categories
    this.contributors = feed.contributors
    this.generator = feed.generator
    this.id = Some(feed.id)
    this.links = feed.links
    this.logo = feed.logo
    this.rights = feed.rights
    this.subtitle = feed.subtitle
    this.title = Some(feed.title)
    this.updated = Some(feed.updated)
  }
  
  override def toString = {
    val sb = new StringBuffer
    sb.append("Authors: ").append(authors.mkString("", ", ", ""))
      .append("\nCategories: ").append(categories.mkString("", ", ", ""))
      .append("\nContributors: ").append(contributors.mkString("", ", ", ""))
      .append("\nGenerator: ").append(generator)
      .append("\nIcon: ").append(icon)
      .append("\nId: ").append(id)
      .append("\nLinks: ").append(links.mkString("", ", ", ""))
      .append("\nLogo: ").append(logo)
      .append("\nRights: ").append(rights)
      .append("\nSubtitle: ").append(subtitle)
      .append("\nTitle: ").append(title)
      .append("\nUpdated: ").append(updated)
      .toString
  }
}

/** Provide a pickler for Source. */
object Source {
  private implicit val atomNs = Uris.atomNs

  lazy val atomSourceContents =
    interleaved(
        rep(atomPerson("author"))
      ~ rep(Category.pickler)
      ~ rep(atomPerson("contributor"))
      ~ opt(Generator.pickler)
      ~ opt(elem("id", text))
      ~ rep(Link.pickler)
      ~ opt(elem("logo", text))
      ~ opt(elem("rights", text))
      ~ opt(atomText("subtitle"))
      ~ opt(atomText("title"))
      ~ opt(elem("updated", dateTime)))

  /** A pickler for Source. */
  lazy val pickler: Pickler[Source] = wrap (elem("source", atomSourceContents)) ({
    case authors ~ cats ~ contribs ~ generator ~ id
         ~ links ~ logo ~ rights ~ subtitle ~ title ~ updated => 
      val e = new Source
      e.authors = authors
      e.categories = cats
      e.contributors = contribs
      e.generator = generator
      e.id = id
      e.links = links
      e.logo = logo
      e.rights = rights
      e.subtitle = subtitle
      e.title = title
      e.updated = updated
      e
  }) (fromSource)

  /** Turn the Source object into a tuple. */
  private def fromSource(e: Source) = (new ~(e.authors, e.categories) 
      ~ e.contributors ~ e.generator ~ e.id ~ e.links ~ e.logo
      ~ e.rights ~ e.subtitle ~ e.title ~ e.updated)
}