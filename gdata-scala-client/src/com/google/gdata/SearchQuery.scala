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
 * Base class for Search queries. Supports 'or', 'and' and negation. Start with
 * SearchQuery.empty which returns a query that matches all, and use the operators
 * to build a more complex query.
 * 
 * Example:
 *   SearchQuery.empty & "football" & !Text("soccer")
 * 
 * It can be turned into a GData query by calling 'toQuery'.
 * 
 * @author Iulian Dragos
 * @see com.google.gdata.Query
 */
sealed abstract class SearchQuery {
  /** Turn this search query into a full query. */
  def toQuery = new Query(this)

  /** Return a query that matches both the current query and the given query. */
  def &(q: SearchQuery): SearchQuery = new SAnd(this, q)
  
  /**
   * Return a query that matches both the current query and the given string.
   * If the string contains spaces, the query will match the exact string by 
   * enclosing it in double quotes.
   */
  def &(t: String): SearchQuery = new SAnd(this, Text(t))
  
  /** Return a query that matches either the current query or the given query. */
  def |(q: SearchQuery): SearchQuery = new SOr(this, q)
  
  /**
   * Return a query that matches either the current query or the given string.
   * If the string contains spaces, the query will match the exact string by 
   * enclosing it in double quotes.
   */
  def |(t: String): SearchQuery = new SOr(this, Text(t))
  
  /** Negate the current query. */
  def unary_! : SearchQuery = this match {
    case SNot(p) => p
    case _ => SNot(this)
  }

  /** 
   * Return a String representation of this query. And is turned into spaces, or is
   * turned into the pipe symbol, strings with spaces are quoted.
   */
  private def buildString(p: SearchQuery): String = p match {
    case SAnd(p, q) => buildString(p) + " " + buildString(q)
    case SOr(p, q)  => buildString(p) + "|" + buildString(q)
    case SNot(q)    => "-" + buildString(q)
    case Text(str) => if (str.contains(" ")) "\"" + str + "\"" else str
    case MatchAll => ""
  }
  
  override def toString =
    buildString(this)
}

/** A query that matches both subqueries. */
case class SAnd(p: SearchQuery, q: SearchQuery) extends SearchQuery

/** A query that matches either on of the subqueries. */
case class SOr(p: SearchQuery, q: SearchQuery) extends SearchQuery

/** A query that matches only when 'p' does not. */
case class SNot(p: SearchQuery) extends SearchQuery

/** A query that matches the exact string 'str'. */
case class Text(str: String) extends SearchQuery

/** 
 * A query that matches everything. All operators are rewritten so that MatchAll behaves
 * like the 'unit' (they return their argument).
 */
case object MatchAll extends SearchQuery {
  override def &(p: SearchQuery) = p
  override def &(str: String) = Text(str)
  override def |(p: SearchQuery) = p
  override def |(str: String) = Text(str)
}

/**
 * Convenience factory object.
 */
object SearchQuery {
  def empty: SearchQuery = MatchAll
}