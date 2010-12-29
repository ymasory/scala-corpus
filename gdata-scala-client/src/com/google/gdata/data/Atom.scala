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

import scala.xml.{NamespaceBinding, TopScope}
import com.google.xml.combinators.{Picklers, HasStore}
import com.google.gdata.data.util.DateTime

/** A text construct, according to the Atom specification. The content is uninterpreted. */
case class Text(tpe: Option[String],
                content: String) extends Tuple2(tpe, content) {
  
  /** A convenience constructor for plain text. */
  def this(content: String) = {
    this(None, content)
  }
}

/** A default, empty text construct. */
object NoText extends Text(None, "")

/** A person construct, according to the Atom specification. */
case class Person(name: String,
                  uri: Option[String],
                  email: Option[String]) extends Tuple3(name, uri, email) with HasStore

/**
 * This object defines common Atom constructs. 
 * See @link http://atomenabled.org/developers/syndication/atom-format-spec.php
 */
object Atom {
  import Picklers._

  implicit private val atomNs = Uris.atomNs

  /**
   * Return a pickler for an element that is a text construct.
   */
  def atomText(elemName: String): Pickler[Text] =
    (wrap (elem(elemName, opt(attr("type",  text)) ~ text))
          (Text) (tuple2Pair))
          
  /** Return a pickler for an element that is a person construct. */
  def atomPerson(elemName: String): Pickler[Person] = elem(elemName,
    makeExtensible(wrap (interleaved(elem("name", text)
        ~ opt(elem("uri", text))
        ~ opt(elem("email", text)))) (Person) (tuple3Pair)))

  /** 
   * Return a pickler for a date time in Atom format (RFC 3339). Atom common
   * attributes (xml:base and xml:lang) are ignored.
   */
  def atomDate(elemName: String): Pickler[DateTime] =
    elem(elemName, dateTime)
}

