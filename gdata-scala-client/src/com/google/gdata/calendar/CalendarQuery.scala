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
package calendar

import com.google.gdata.data.util.DateTime

/**
 * A calendar query. It adds methods for specifying whether recurring events should
 * be expanded, which events should be returned, the timezone for the returned times, etc. 
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/calendar/reference.html#Parameters
 */
class CalendarQuery extends Query {
  /**
   * Specifies how to sort events in the search result set. Valid values for this 
   * parameter are 'lastmodified' and 'starttime'. Default is 'lastmodified'. 
   */
  def orderBy(ordering: String): this.type =
    addParam("orderby", ordering)
  
  /** Request all future events. */
  def futureEvents(b: Boolean): this.type = 
    addParam("futureevents", String.valueOf(b))
  
  /** 
   * Interval for which recurring events are expanded. The beginning of the interval 
   * is inclusive, the end is exclusive. 
   */
  def expandRecurrence(start: DateTime, end: DateTime): this.type = {
    expandStart(start)
    expandEnd(end)
  }
  
  /** Start date for recurrent event expansion (inclusive). */
  def expandStart(start: DateTime): this.type =
    addParam("recurrence-expansion-start", start.toString)
  
  /** End date for recurrent event expansion (exclusive). */
  def expandEnd(end: DateTime): this.type =
    addParam("recurrence-expansion-end", end.toString)
  
  /** Indicates whether recurring events should be expanded or represented as a single event. */
  def singleEvents(b: Boolean): this.type =
    addParam("singleevents", String.valueOf(b))
  
  /** 
   * Specifies direction of sorting. Valid values are 'ascend', 'ascending', 'a' and
   * 'descend', 'descending', 'd'.
   */
  def order(direction: String): this.type = 
    addParam("sortorder", direction)
  
  /**
   * Together with start-max creates a timespan such that only events that are 
   * within the timespan are returned. If not specified, default start-min is 
   * 1970-01-01. This bound is inclusive.
   */
  def startMin(start: DateTime): this.type =
    addParam("start-min", start.toString)
  
  /**
   * Together with start-min creates a timespan such that only events that are 
   * within the timespan are returned. If not specified, default start-max is 
   * 2031-01-01. This bound is exclusive.
   */
  def startMax(end: DateTime): this.type =
    addParam("start-max", end.toString)

  /** 
   * Return events who start in the given interval. The interval is inclusive at the
   * beginning and exclusive at the end.
   */
  def startBetween(start: DateTime, end: DateTime): this.type = {
    startMin(start)
    startMax(end)
  }
  
  /**  
   * The current timezone. If not specified, times are returned in UTC.
   * Replace all spaces with underscores (e.g. "ctz=America/Los_Angeles").
   */
  def timeZone(tz: String): this.type = 
    addParam("ctz", tz)
}
