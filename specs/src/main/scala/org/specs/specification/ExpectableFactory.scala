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
import org.specs.matcher._

/**
 * This trait defines implicit definitions which are used to create Expectable objects
 * and associate them with the latest defined example<br>
 * Usage: <code>
 * 2.1 must beCloseTo(2.0, .1)
 * </code>or<br><code
 * theDouble(2.1) must beCloseTo(2.0, .1)
 * </code><p>
 *
 * Then StringMatchers are expecting values with String as an upper bound so that effectively only String instances
 * will be used with the implicit def (only solution found to make it all work, to my current understanding)
 */
trait ExpectableFactory extends ExampleExpectationsListener with SuccessValues with FailureFactory {

  /** create a FailureExceptionWithResult when an expectation is failing */
  def createFailure[T](message: String, result: Result[T]): Throwable with HasResult[T] = new FailureExceptionWithResult(message, result)

  /** implicit transformation of an object into one supporting AnyMatcher matchers */
  implicit def theValue[A](value: =>A): Expectation[A] = {
    val a = new Expectation(value)
    a.setSuccessValueToString(successValueToString _)
    a.setExpectationsListener(expectationsListener)
    a.setFailureFactory(this)
  }

  /** implicit transformation of a String into an object supporting String matchers */
  implicit def theString(value: =>String) = {
    val a = new StringExpectable(value.toString)
    a.setSuccessValueToString(successValueToString _)
    a.setExpectationsListener(expectationsListener)
    a.setFailureFactory(this)
  }

  /**
   * implicit transformation of a block returning Nothing.
   * This is necessary when testing thrown exceptions <pre>stream.close must throwA(new IOException)</pre>
   */
  implicit def theBlock(value: =>Nothing): Expectation[Nothing] = {
    val a = new Expectation(value)
    a.setSuccessValueToString(successValueToString _)
    a.setExpectationsListener(expectationsListener)
    a.setFailureFactory(this)
  }
  /** implicit transformation of an Iterable[String] into an object supporting IterableString matchers */
  implicit def theStrings(value: =>Iterable[String]): IterableStringExpectable = {
    val a = new IterableStringExpectable(value)
    a.setSuccessValueToString(successValueToString _)
    a.setExpectationsListener(expectationsListener)
    a.setFailureFactory(this)
  }

  /** implicit transformation of an Iterable into an object supporting Iterable matchers */
  implicit def theIterable[I <: AnyRef](value: =>Iterable[I]): IterableExpectable[I] = {
    val a = new IterableExpectable(value)
    a.setSuccessValueToString(successValueToString _)
    a.setExpectationsListener(expectationsListener)
    a.setFailureFactory(this)
  }
}
class DelegatedExpectableFactory(var delegate: ExpectableFactory) extends ExpectableFactory {
  def forExample: Examples = delegate.forExample
  def lastExample: Option[Examples] = delegate.lastExample
}
class DefaultExpectableFactory extends ExpectableFactory {
  private val defaultExample = new Example("", DefaultLifeCycle)
  def forExample: Example = defaultExample
  def lastExample: Option[Example] = Some(forExample)
}