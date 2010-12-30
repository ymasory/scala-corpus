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

/**
 * Trait declaring the ability to add a new expectation to an Example.
 */
trait ExpectationsListener {
  def addExpectation: Examples
  /**
   * Adds an isExpectation method to any block of code (mock expectation, scalacheck property) to better count the number of expectations
   */
  implicit def anyToExpectationCounter[T](a: =>T) = new ExpectationCounter(a)
  /**
   * Declares a block of code to count as an expectation
   */
  def isExpectation[T](a: =>T) = anyToExpectationCounter(a).isExpectation
  /**
   * Adds an isExpectation method to any block of code to better count the number of expectations
   */
  class ExpectationCounter[T](a: =>T) {
    /** adds an expectation to the ExpectationListener trait */
    def isExpectation = { addExpectation; a }
  }
}
/**
 * Trait adding the new expectation to an example, creating one if necessary.
 */
trait ExampleExpectationsListener extends ExpectationsListener {
  
  /** 
   * By default the listener is this. However it may be changed to another listener
   * when the specification is cloned for isolated execution
   * @see SpecificationExecutor
   */
  var expectationsListener: ExampleExpectationsListener = this

  def addExpectation: Examples = addExpectation(None)

  /**
   * Add an expectation to the last created example.
   * If there is none, create a new one with the forExample function.
   *
   * If an expectable is given, then this expectable should be assigned the created example.
   * This is the case when adding an expectation for a one-liner jmock expectation:<pre>
   *   // in the expects method a new expectation is created with isExpectation
   *   // see org.specs.mock.JMocker
   *   classOf[MyClass].expects(one(_).method) in { _.method }
   * </pre>
   */
  def addExpectation[T](expectable: Option[Expectable[T]]): Examples = {
    expectationsListener.lastExample match {
      case None => {
        val ex = expectationsListener.forExample.addExpectation
        expectable.map(_.setExample(ex))
        ex
      }
      case Some(e) => {
        e.addExpectation
      }
    }
  }
  /**
   * create a new example.
   */
  def forExample: Examples

  /**
   * retrieve the last created example.
   */
  def lastExample: Option[Examples]
}
/**
 * Trait adding an expectation on a default unused example.
 *
 * It is used to provide a default behavior when examples are not necessary, i.e. when using SpecsMatchers only.
 */
trait DefaultExampleExpectationsListener extends ExampleExpectationsListener {
  private val defaultExample = new Example("unused")
  override def addExpectation: Examples = forExample.addExpectation
  /**
   * Here we don't try to set the expectation on the default example since we want the expectation to execute
   * right away.
   */
  override def addExpectation[T](expectable: Option[Expectable[T]]): Examples = forExample.addExpectation
  def lastExample: Option[Examples] = Some(defaultExample)
  def forExample = defaultExample
}
