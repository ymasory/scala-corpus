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


package com.google.gdata

import com.google.gdata.data.util.DateTime

import java.net.URLEncoder

/**
 * Base class for GData queries. It has a category part and a search query part. 
 * Categories are added using the '/' operator. The search query is added using
 * 'suchThat'. Retrieve the URL by calling 'mkUrl(base)'. It returns a URL looking
 * like 'base/-/categories/../lastCategory?q=<searchquery>'. It takes care of URL
 * encoding.
 * 
 * It handles the default GData parameters (author, alt, updated-min, updated-max, 
 * published-min, published-max, start-index, max-results). @see <a href=
 * "http://code.google.com/apis/gdata/reference.html#Queries">GData queries</a>.
 * 
 * Subclasses should override 'searchName' to change 'q' to something else. New
 * parameters should be implemented as methods in terms of 'addParam'.
 * 
 * @author Iulian Dragos
 */
class Query(var searchQuery: SearchQuery) {
  
  def this() {
    this(MatchAll)
  }
  
  def &(q: SearchQuery) = searchQuery &= q
  def |(q: SearchQuery) = searchQuery |= q
  
  /** Add a category to this query. */
  def /(q: CategoryQuery): this.type = {
    categories = URLEncoder.encode(q.toString, "UTF-8") :: categories
    this
  }
  
  /** Add a category to this query. */
  def /(cat: String): this.type = {
    categories = URLEncoder.encode(cat, "UTF-8") :: categories
    this
  }
  
  /** Set the default text query. This is usually the 'q' parameter. */
  def matching(q: SearchQuery): this.type = {
    searchQuery = q
    this
  }
  
  /** Return the URL query given the base URL. The URL should not have a trailing '/'. */
  def mkUrl(base: String): String = {
    val buf = new StringBuilder
    buf.append(base)
    addCategories(buf)
    if (searchQuery != MatchAll) addParam(searchName, searchQuery.toString)
    addParams(buf).toString
  }
  
  /** 
   * The parameter name for text searches. This is used for the deafult seach query, 
   * introduced by @code suchThat.
   */
  def searchName: String = "q"
  
  /** Additional parameters as name-value pairs. Values are url-encoded. */
  private var params: List[(String, String)] = Nil
  
  /** Categories in this query. Kept as url-encoded strings. */
  private var categories: List[String] = Nil
  
  /** 
   * Add a custom parameter to this query. Use method calls for standard parameters. 
   * Subclasses should use this method to implement custom search parameters. It
   * url-encodes the given value.
   */
  def addParam(name: String, value: String): this.type = {
    params = (name, java.net.URLEncoder.encode(value, "UTF-8")) :: params
    this
  }
  
  /** Returns entries where the author name and/or email address match the given string. */
  def author(a: String): this.type =
    addParam("author", a)
  
  /** 
   * Return in the given alternative representation. Valid alternatives are 'atom' and 'rss'.
   * 
   * @throws IllegalArgumentException if something other than rss and atom is given.
   */
  def alt(alt: String): this.type = {
    if (alt != "rss" && alt != "atom") 
      throw new IllegalArgumentException("Invalid alternative: " + alt
          + " only rss and atom allowed.")
    addParam("alt", alt)
  }
  
  /** Return entries updated later (inclusive) than the given date. */
  def updatedMin(dt: DateTime): this.type = {
    addParam("updated-min", dt.toString)
  }

  /** Return entries updated before (exclusive) than the given date. */
  def updatedMax(dt: DateTime): this.type = {
    addParam("updated-max", dt.toString)
  }

  /** Return entries published later (inclusive) than the given date. */
  def publishedMin(dt: DateTime): this.type = {
    addParam("published-min", dt.toString)
  }

  /** Return entries published before (exclusive) than the given date. */
  def publishedMax(dt: DateTime): this.type = {
    addParam("published-max", dt.toString)
  }
  
  /** 1-based index of the first result to be retrieved. */
  def startIndex(idx: Int): this.type = {
    addParam("start-index", String.valueOf(idx))
  }
  
  /** Maximum number of results to be retrieved. */
  def maxResults(n: Int): this.type = {
    addParam("max-results", String.valueOf(n))
  }
  
  private def addCategories(buf: StringBuilder): StringBuilder = {
    if (!categories.isEmpty)
      buf.append(categories.reverse.mkString("/-/", "/", ""))
    buf
  }
  
  /** Add parameters to the given buffer. */
  private def addParams(buf: StringBuilder): StringBuilder = {
    if (!params.isEmpty) {
      params.reverse.foldLeft(buf.append('?')) { 
        case (buf, (n, v)) => 
          buf.append(n)
          if (v.length > 0)
            buf.append('=').append(v).append('&')
          buf
      }
      buf.deleteCharAt(buf.length - 1) // remove the last '&'
    } else
      buf
  }
}

object Query {
  def empty = new Query
}