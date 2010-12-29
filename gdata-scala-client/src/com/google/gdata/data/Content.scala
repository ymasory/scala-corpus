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

import com.google.xml.combinators.{Picklers, ~, XmlStore}

import scala.xml.{NodeSeq, Utility}

/**
 * A content element as defined by the Atom syndication format.
 * 
 * @see http://atomenabled.org/developers/syndication/atom-format-spec.php#element.content
 */
abstract class Content(tpe: String)

/** A plain text content. */
case class TextContent(var text: String) extends Content("text")

/** An Html content. */
case class HtmlContent(var text: String) extends Content("html")

/** A Xhtml content. */
case class XhtmlContent(var div: NodeSeq) extends Content("xhtml")

/** An out of line content (linked to an external URL). */
case class OutOfLineContent(var src: String, var tpe: String) extends Content(tpe)

/** Provide picklers for content elements. */
object Content {
  implicit val atomNs = Uris.atomNs
  import Picklers._
  
  val pickler: Pickler[Content] = {
    val content = elem("content", 
        opt(attr("type", text)) ~ opt(attr("src", text)) ~ xml)
        
    def toContent(parsed: Option[String] ~ Option[String] ~ NodeSeq): Content = 
      parsed match {
        case Some("text") ~ None ~ ctent => TextContent(ctent.toString)
        case Some("html") ~ None ~ ctent => HtmlContent(ctent.toString)
        case Some("xhtml") ~ None ~ ctent => XhtmlContent(ctent)
        case Some(tpe) ~ Some(src) ~ NodeSeq.Empty => OutOfLineContent(src, tpe)
        case _ ~ _ ~ ctent => TextContent(ctent.toString) // this forgives some malformed entries!
      }
    
    def fromContent(c: Content) = c match {
      case TextContent(plainText) =>
        new ~(new ~(Some("text"), None), NodeSeq.fromSeq(Utility.parseAttributeValue(plainText)))
      case HtmlContent(htmlText) => 
        new ~(new ~(Some("html"), None), NodeSeq.fromSeq(Utility.parseAttributeValue(htmlText)))
      case XhtmlContent(xhtml) => 
        new ~(new ~(Some("xhtml"), None), xhtml)
      case OutOfLineContent(src, tpe) =>
        new ~(new ~(Some(tpe), Some(src)), NodeSeq.Empty)
    }
    wrap (content) (toContent) (fromContent)
  }
}
