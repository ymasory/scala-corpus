/* Copyright (C) 2008-2010 University of Massachusetts Amherst,
   Department of Computer Science.
   This file is part of "FACTORIE" (Factor graphs, Imperative, Extensible)
   http://factorie.cs.umass.edu, http://code.google.com/p/factorie/
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */



package cc.factorie.util

//import cc.factorie.util.Index

import scala.collection.mutable._;

trait Trie[T] extends (Seq[T] => Boolean) {
  private val root = new HashMap[T, Int]

  def add(seq: Seq[T]): Unit

  def present(seq: Seq[T], index: Int): Boolean

  /**Constructs an Index from some elements. */
  def apply[T](iterable: Iterable[T]): Index[T] = {
    val index = new Index[T] {};
    // read through all elements now -- don't lazily defer evaluation
    for (element <- iterable) {
      index.index(element);
    }
    return index;
  }

}
