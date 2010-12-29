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


package com.google.util

/**
 * An object gathering small methods used from different places.
 * 
 * @author Iulian Dragos
 */
object Utility {
  
  /** Print an optional value when not empty. */
  def printOptional[A](sb: StringBuilder, name: String, v: Option[A]) = {
    if (v.isEmpty) sb else sb.append(name).append(": ").append(v.get)
  }
  
  /** 
   * Adds leading zeroes to a given int to fill 'digits', and appends it to the StringBuffer.
   * Assumes positive integers.
   */
  def padInt(sb: StringBuilder, n: Long, digits: Int): StringBuilder = {
    val str = n.toString
    var delta = digits - str.length
    
    assert(delta >= 0, "Not enough digits to pad number: " + n + " on " + digits + " digits")
    
    while (delta > 0) {
      sb.append('0')
      delta = delta - 1
    }
    sb.append(str)
  }
  
  def option[A <: AnyRef](a: A): Option[A] =
    if (a eq null) None else Some(a)
}
