/**
 * Copyright (c) 2007-2010 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.form
import org.specs.util.Property

class EntityLineProp[E, T](override val label: String,
                    expectedValue: Property[T],
                    function: E => T, 
                    val entity: Property[E], constraint: Option[MatcherConstraint[T]]) extends 
  LineProp[T](label, expectedValue, entity.map(function(_)), constraint) {

  override def copy: EntityLineProp[E, T] = {
    val e = Property[E]()
    val p = new EntityLineProp[E, T](label, expectedValue, function, e, 
                                     constraint.map(c => new MatcherConstraint(e.map(function(_)).optionalValue, c.executor)))
    super.copy(p)
    p
  }
}
object EntityLineProp {
  def apply[E, T](label: String, expected: =>T, function: E => T, entity: => E, constraint: MatcherConstraint[T]) = {
    new EntityLineProp(label, Property(expected), function, Property(entity), Some(constraint))
  }
  
}

