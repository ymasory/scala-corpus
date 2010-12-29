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
 * This class represents a period of time or an instant. If endTime is not specified, it is 
 * considered an instant in time when the startTime has a 'time' component, or a whole-day
 * if startTime is only a date.
 * 
 * @author Iulian Dragos
 * @see http://code.google.com/apis/gdata/elements.html#gdWhen
 */
class When {
  /** 
   * Event start time. If the timezone is not specified, the observer's local timezone is assumed.
   */
  var startTime: Option[DateTime] = None

  /** 
   * Event end time. If the timezone is not specified, the observer's local timezone is assumed.
   */
  var endTime: Option[DateTime] = None
  
  /** A simple string value that can be used as a representation of this time period. */
  var valueString: Option[String] = None
  
  /** Reminders for this time. */
  var reminders: List[Reminder] = Nil
  
  /** Convenience factory method for common cases.  */
  def this(startTime: DateTime, endTime: DateTime) {
    this()
    this.startTime = Some(startTime)
    this.endTime = Some(endTime)
  }
}

object When {
  import Picklers._
  
  /** Return a pickler for When. */
  lazy val pickler: Pickler[When] = {
    val contents = elem("when", 
        opt(attr("startTime", dateTime(true)))
          ~ opt(attr("endTime", dateTime(true)))
          ~ opt(attr("valueString", text))
          ~ rep(Reminder.pickler))(Uris.gdNs)
    
    wrap (contents) ({
      case startTime ~ endTime ~ valueString ~ reminders =>
        val when = new When
        when.startTime = startTime
        when.endTime = endTime
        when.valueString = valueString
        when.reminders = reminders
        when
    }) (fromWhen _)
  }
  
  private def fromWhen(when: When) = 
    (new ~(when.startTime, when.endTime) ~ when.valueString ~ when.reminders)
}
