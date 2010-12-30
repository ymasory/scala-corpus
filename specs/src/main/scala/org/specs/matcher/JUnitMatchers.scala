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
import _root_.junit.framework.AssertionFailedError
import org.specs.specification.{ Result, HasResult }
import org.specs._

/**
 * This trait provides the possibility to use specs matchers in a JUnit test case.
 * In case of a failure, a (subclass of) AssertionFailedError is thrown to be catched by JUnit runners.
 */
trait JUnitMatchers extends SpecsMatchers {
  override def createFailure[T](message: String, result: Result[T]): Throwable with HasResult[T] = new JUnitFailureExceptionWithResult(message, result)
}
/**
 * Specialized AssertionFailedError holding a result in order to cope with the case where matchers are being or-ed together
 */
case class JUnitFailureExceptionWithResult[T](message: String, @transient result: Result[T]) extends AssertionFailedError(message) with HasResult[T]
