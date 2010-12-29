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
package kinds

import com.google.gdata.data.util.DateTime
import com.google.xml.combinators.{Picklers, ~}

/**
 * A reminder, usually found inside a 'when' element.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdReminder
 */
case class Reminder(
    /** Absolute time at which the reminder should be issued. */
    absoluteTime: Option[DateTime],
    
    /** The notification method the reminder should use, like 'alert', 'email, 'sms'. */
    method: Option[String],
    
    /**
     * Period of time before gd:when/@startTime when a reminder should be issued. 
     * If the parent entity's target time is a date rather than a specific time, 
     * then these attributes are relative to midnight (00:00) on that date 
     */
    days: Option[Int],
    
    /**
     * Period of time before gd:when/@startTime when a reminder should be issued. 
     * If the parent entity's target time is a date rather than a specific time, 
     * then these attributes are relative to midnight (00:00) on that date 
     */
    hours: Option[Int],
    
    /**
     * Period of time before gd:when/@startTime when a reminder should be issued. 
     * If the parent entity's target time is a date rather than a specific time, 
     * then these attributes are relative to midnight (00:00) on that date 
     */
    minutes: Option[Int])

object Reminder {
  import Picklers._
  
  /** A Pickler for reminder objects. */
  lazy val pickler: Pickler[Reminder] = {
    val ctents = elem("reminder", opt(attr("absoluteTime", dateTime))
        ~ opt(attr("method", text)) ~ opt(attr("days", intVal))
        ~ opt(attr("hours", intVal)) ~ opt(attr("minutes", intVal)))(Uris.gdNs)
    
    wrap (ctents) (Reminder.apply) (fromReminder)
  }
  
  private def fromReminder(r: Reminder) =
    new ~(r.absoluteTime, r.method) ~ r.days ~ r.hours ~ r.minutes
  
}
