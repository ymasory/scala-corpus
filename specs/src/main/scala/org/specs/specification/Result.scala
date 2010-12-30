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
package org.specs.specification
import org.specs.matcher.Matcher
import org.specs.matcher.BeNull
import org.specs.util.Property

/** 
 * Result of a match
 * 
 * This object carries the Expectable object, in order to apply further matches if necessary.
 * 
 * It has a display function which can be used to set the toString function to an empty string,
 * in the case of Literate specifications where we want to embed expectations without having their
 * result printed in the specification text.
 * 
 * It can also be set to "already ok" in order to court-circuit any further matches in the case of "or-ed"
 * matchers with a successful first match.
 * 
 */
class Result[T](expectable: => Expectable[T], display: SuccessValue => String) extends SuccessValue {
  private var isAlreadyOk = false
  def setAlreadyOk() = { isAlreadyOk = true; this }
  def setNotAlreadyOk() = { isAlreadyOk = false; this }
  override def toString = display(this)
  def nextSignificantMatchMustFail() = { expectable.nextSignificantMatchMustBeNegated(); this }
  def matchWith[S >: T](m: => Matcher[S]) = if (isAlreadyOk) this else expectable.applyMatcher(m)
  def matchWithMatcher(m: => Matcher[T]) = if (isAlreadyOk) this else expectable.applyMatcher(m)
  def be(m: => Matcher[T]) = {
    if (null eq m)
      matchWith(new BeNull[T])
    else
      matchWith(m)
  }
  def have(m: => Matcher[T]) = matchWith(m)
  def apply(m: => Matcher[T]) = matchWith(m)
  def and(m: => Matcher[T]) = matchWith(m)
  def a(m: => Matcher[T]) = matchWith(m)
  def an(m: => Matcher[T]) = matchWith(m)
  def the(m: => Matcher[T]) = matchWith(m)
  def valueProperty = Property(expectable.valueProperty())
}
/**
 * Trait marking anything that holds a Result. It is used to find a commonality between Specs and JUnit failures
 * which are holding a Result.
 */
trait HasResult[T] {
  val result: Result[T]
}
