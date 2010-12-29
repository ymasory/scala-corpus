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

import com.google.xml.combinators.{Picklers, ~}

/**
 * A gd:rating element. Represents a numeric rating of the enclosed entity.
 * 
 * @param average The average value of the ratings.
 * @param min     The rating scale minimum.
 * @param max     The rating scale maximum.
 * @param numRaters The number of ratings taken account by the average value.
 * @param value   Rating value.
 */
case class Rating(average: Option[Double], min: Int, max: Int, 
    numRaters: Option[Int], rel: String, value: Option[Int]) {
  
  override def toString = {
    val sb = new StringBuilder
    average match {
      case Some(avg) => sb.append(avg)
      case None => value match {
        case Some(v) => sb.append(v)
        case None => ()
      }
    }
    sb.append('/').append(max)
    if (!numRaters.isEmpty)
       sb.append(" (").append(numRaters.get).append(" voters").append(')')
    sb.toString
  }
}

object Rating {
  import Uris.gdNs
  import Picklers._
  
  /** A pickler for Rating classes. */
  def pickler: Pickler[Rating] = 
    (wrap (elem("rating", opt(attr("average", doubleVal)) ~ attr("min", intVal)
        ~ attr("max", intVal) ~ opt(attr("numRaters", intVal))
        ~ default(attr("rel", text), "overall") ~ opt(attr("value", intVal)))(gdNs))
        (Rating.apply) (fromRating))
  
  private def fromRating(r: Rating) = (new ~(r.average, r.min) ~ r.max
      ~ r.numRaters ~ r.rel ~ r.value)
}
