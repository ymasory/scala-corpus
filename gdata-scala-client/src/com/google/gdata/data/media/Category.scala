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
 * A media:category element, as defined by Media RSS
 * 
 * @param scheme Categorization scheme (defaults to "http://search.yahoo.com/mrss/category_schema").
 * @param label  User-friendly label for the categorization scheme.
 * @param value  The category itself.
 * 
 * @see http://search.yahoo.com/mrss
 * @author Iulian Dragos 
 */
case class Category(scheme: String, label: Option[String], value: String)

object Category {
  final val DEFAULT_SCHEMA = "http://search.yahoo.com/mrss/category_schema"
  
  val pickler: Pickler[Category] = 
    (wrap (elem("category", 
         default(attr("scheme", text), DEFAULT_SCHEMA)
       ~ opt(attr("label", text)) ~ text)(Uris.mediaNs))
       (Category.apply)
       (fromCategory))
  
  private def fromCategory(c: Category) =
    new ~(c.scheme, c.label) ~ c.value
}
