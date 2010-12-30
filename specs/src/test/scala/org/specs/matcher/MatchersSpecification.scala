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
package org.specs.matcher
import scala.collection.mutable.Queue
import org.specs.runner._
import org.specs.Sugar._
import org.specs.specification._
import org.specs.execute._
import org.specs._

class MatchersSpecification extends SpecificationWithJUnit with ExpectationMatchers {
  var reported: Example = new Example("this example serves as a stub to collect failure messages", new Sus("", this))
  // an expression which knows how much time is had been evaluated
  case class exp[T](var a: T) { var evaluationsNb: Int= 0; def evaluate = {evaluationsNb += 1; a} }

  // a matcher which checks that a matcher is not evaluating twice the value to evaluate
  def evalOnce[T](a : exp[T]) = new Matcher[Matcher[T] ] {
    def apply(m: =>Matcher[T]) = ({m.apply(a.evaluate); a.evaluationsNb == 1}, "ok", "ko")
  }
}
trait ExpectationMatchers { this: Specification =>
  def failWith(message: String) = is_==(message)
  def failWithMatch(pattern: String) = beMatching(pattern)
  def expectation(value: => Any): String = {
    try {
      value
    } catch {
      case FailureException(message) => return message
      case t: Throwable => throw t
    }
    return "this expectation has not failed"
  }
}