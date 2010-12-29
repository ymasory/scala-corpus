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


package com.google.util;

import collection.mutable.{Set, Map}
 
/** This class is typically used as a mixin. It turns maps which map <code>A</code>
 *  to <code>Set[B]</code> objects into multi maps which map <code>A</code> to
 *  <code>B</code> objects. This class allows some choice of the concrete type
 *  of Set, while the standard one in the Scala library is fixing it to HashSet.
 *  Use this when your elements have no hashCode method, like NodeSeq in the XML
 *  libraries.
 *  <p/>
 *  Adapted from the Scala standard libraries. Parked here until it moves to the
 *  standard library.
 *
 *  @author  Matthias Zenger, Iulian Dragos
 *  @version 1.0, 02/19/2008
 */
trait MultiMap[A, B, TSet <: Set[B]] extends Map[A, TSet] {
  protected def makeSet: TSet

  def add(key: A, value: B): Unit = get(key) match {
    case None =>
      val set = makeSet
      set += value
      this(key) = set
    case Some(set) =>
      set += value
  }

  def remove(key: A, value: B) = get(key) match {
    case None =>
    case Some(set) => set -= value
  }

  def entryExists(key: A, p: B => Boolean): Boolean = get(key) match {
    case None => false
    case Some(set) => set exists p
  }
}
