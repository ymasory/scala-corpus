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

;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/


import scala.collection.mutable.WeakHashMap;
import java.lang.ref.WeakReference;

/**
 * Class that mimics Java's string interner, but for anything.
 * Threadsafe.
 *
 * @author dlwh
 */
class Interner[T] extends (T => T) { 
  override def apply(t: T) = intern(t);

  def intern(t: T): T = synchronized {
    inner.getOrElseUpdate(t, new WeakReference[T](t)).get;
  }

  def clear() = inner.clear();
  def size = inner.size;

  def internAll(c: Iterable[T]) = c map apply

  //def internAll(c: Iterable[T]) = c map apply

  def internAll(c: List[T]) = c map apply

  def internAll(c: Array[T]) = c map apply

  def internAll(c: Set[T]) = c map apply

  def internKeys[V](c: scala.collection.Map[T, V]) = {
    Map[T, V]() ++ c.map {case (k, v) => (intern(k), v)}
  }

  def internValues[K](c: scala.collection.Map[K, T]) = {
    Map[K, T]() ++ c.map {case (k, v) => (k, intern(v))}
  }

  private val inner = new WeakHashMap[T, WeakReference[T]];
}

object Interner {
  private val typedInterners = new scala.collection.mutable.HashMap[Class[_], Interner[_]] {
    override def default(c: Class[_]) = getOrElseUpdate(c, new Interner[Any]);
  }

  def apply[T](implicit m: scala.reflect.Manifest[T]) = forClass[T](m.erasure.asInstanceOf[Class[T]]);

  def forClass[T](c: Class[T]) = typedInterners(c).asInstanceOf[Interner[T]];
}
