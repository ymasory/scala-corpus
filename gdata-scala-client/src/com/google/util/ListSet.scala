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

import scala.collection.mutable.Set
 
/**
 * A mutable Set backed by a List. Inserts are constant time, removes and contains
 * are linear. Use when sets are supposed to remains small, and a HashSet would not
 * fit the bill (for instance, no implementation of hashCode). 
 * 
 * @author Iulian Dragos
 * @see scala.xml.NodeBuffer
 */
class ListSet[A] extends Set[A] {
  private var elems: List[A] = Nil
  
  def this(elems: List[A]) {
    this()
    this.elems = elems 
  }
  
  def +=(elem: A): this.type = {
    if (!elems.contains(elem))
      elems = elem :: elems
    this
  }
  
  def -=(elem: A): this.type = {
    elems = elems.filterNot(_ == elem)
    this
  }
  
  override def clone = new ListSet(elems)
  
  override def size = elems.length
  def contains(elem: A) = elems.contains(elem)
  def iterator = elems.iterator
}
