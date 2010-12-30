/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest

/**
 * A <code>Suite</code> class that takes a <code>List[Suite]</code> parameter, which overrides
 * the <code>nestedSuites</code> method of trait <code>Suite</code>.
 *
 * @param nestedSuites a <code>List</code> of nested <code>Suite</code>s.
 *
 * @throws NullPointerException if <code>suite</code> is <code>null</code>.
 *
 * @author Bill Venners
 */
class SuperSuite(override val nestedSuites: List[Suite]) extends Suite {

/*
this is wierd. nestedSuites is the only instance data in this class, but it diesn't
seem to make sense to say two suites are the same if they have the same nested suites.
I guess if they have the same class and have the same nested suites, and subtypes agree,
then they are equal.
*/
/*
  override def equals(other: Any) = other match {
    case that: SuperSuite => (this.nestedSuites == that.nestedSuites) && (that isComparable this)
    case _ => false
  }

  def isComparable(other: Any) = other.isInstanceOf[SuperSuite]

  override def hashCode = nestedSuites.hashCode * 41
*/
}
// This is interesting. Seems I could define a default hashcode based on
// properties that are actually methods in the trait. Like nestedSuites. Wierd.
