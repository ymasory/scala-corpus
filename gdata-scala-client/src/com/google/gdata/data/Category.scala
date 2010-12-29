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
import scala.xml.{NamespaceBinding, TopScope}

/**
 * An Atom category, as defined by the Atom spec.
 * 
 * @see http://atomenabled.org/developers/syndication/atom-format-spec.php
 */
case class Category(term: String, scheme: Option[String], label: Option[String]) {
  /** Convenience constructor for a category with no label. */
  def this(term: String, scheme: String) {
    this(term, Some(scheme), None)
  }
}

/** Provides the picklers for Category elements. */
object Category {
  import Picklers._
  implicit val atomNs = Uris.atomNs

  val pickler: Pickler[Category] =
    (wrap(elem("category",
        attr("term", text) ~ opt(attr("scheme", text)) ~ opt(attr("label", text))))
        (Category.apply) (fromCategory))
  
  private def fromCategory(v: Category) = new ~(v.term, v.scheme) ~ v.label
}

