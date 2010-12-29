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

/**
 * A base class for category queries. It supports 'or' and negation. It is used
 * to build complex category queries. This class makes little sense on its own,
 * most users should use @link com.google.gdata.Query.
 * 
 * @author Iulian Dragos
 * @see com.google.gdata.Query
 * @see com.google.gdata.SearchQuery
 */
abstract class CategoryQuery {
  /** Match either this category or 'q'. */
  def |(q: CategoryQuery): CategoryQuery = new COr(this, q)
  
  /** Match either this category or 'cat'. */
  def |(cat: String): CategoryQuery = new COr(this, new Category(cat))
  
  /** Match everything except this category. */
  def unary_! : CategoryQuery = this match {
    case CNot(p) => p
    case _ => CNot(this)
  }
  
  /** Append this category query to the given StringBuilder. */
  def mkString(buf: StringBuilder): StringBuilder = {
    mkString(this, buf)
  }
  
  /** 
   * Return a string representation of this CategoryQuery. Categories are separated by
   * '/'. It has no trailing '/'.
   */
  private def mkString(p: CategoryQuery, buf: StringBuilder): StringBuilder = p match {
    case COr(p, q)  => 
      mkString(p, buf).append('|')
      mkString(q, buf)
    case CNot(q)    => 
      buf.append('-')
      mkString(q, buf)
    case Category(None, term) => 
      buf.append(term)
    case Category(Some(scheme), term) =>
      buf.append('{').append(scheme).append('}').append(term)
    case AllCategories =>
      buf
  }
  
  override def toString =
    mkString(new StringBuilder).toString
}

/** A category with an optional scheme. */
case class Category(scheme: Option[String], term: String) extends CategoryQuery {
  def this(term: String) {
    this(None, term)
  }
}

/** A query that matches either 'p' or 'q'. */
case class COr(p: CategoryQuery, q: CategoryQuery) extends CategoryQuery

/** A query that matches when 'p' doesn't. */
case class CNot(p: CategoryQuery) extends CategoryQuery

/** A query that matches all categories. */
case object AllCategories extends CategoryQuery {
  override def |(p: CategoryQuery) = p
}

/**
 * Convenience factory object.
 */
object CategoryQuery {
  def empty: CategoryQuery = AllCategories
  
  def cat(term: String) = new Category(term)
  def cat(scheme: String, term: String) = new Category(Some(scheme), term)
}

