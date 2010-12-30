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
import org.specs.matcher._

/**
 * Base class for constraints executed on an expected value.
 * 
 * Subclasses include MatcherConstraint (uses a matcher), FunctionConstraint (uses a function), AnyConstraint (uses a blockk.)
 */
abstract class Constraint[T]() extends {
  def execute(expected: Option[T])
}
/**
 * A MatcherConstraint uses a matcher to evaluate an expected value.
 * 
 * By default the matcher is the BeEqualTo matcher, but it can be changed with the matchesWith method:
 * <code>
 * matcherConstraint.matchesWith(be_>=(_))
 * </code>
 * 
 * The executor function is usually an expression like: <code>(m: Matcher[T]) => actual must m</code>
 * which will actually trigger the constraint. 
 */
class MatcherConstraint[T](actual: =>Option[T], val executor: Function2[T, Matcher[T], Any]) extends Constraint[T] {
  private var matcher: T => Matcher[T] = (t: T) => new BeEqualTo(t)
  def matchesWith(m: T => Matcher[T]) = {
    matcher = m
    this
  }
  /**
   * execute the constraint by applying the matcher function to the expected value
   * and passing this to the executor function.
   */
  def execute(expected: Option[T]) = expected.map { exp => 
    actual.map(executor(_, matcher(exp))) 
  }
}
/**
 * This general constraint uses a function taking an actual valuen and an expected value to do the match.
 */
case class FunctionConstraint[T](actual: T, executor: (T, T) => Any) extends Constraint[T]  {
  def execute(expected: Option[T]) = expected.map(executor(actual, _))
}
/**
 * This class represents an arbitrary constraint which may be executed when a Prop is executed.
 * 
 * It doesn't use the provided expected value.
 */
case class AnyConstraint[T](executor: () => Any) extends Constraint[T] {
  def execute(expected: Option[T]) = executor()
}
