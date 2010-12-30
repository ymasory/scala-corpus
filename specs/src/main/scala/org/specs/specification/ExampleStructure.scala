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
import org.specs.execute.{ DefaultResults, FailureException, SkippedException }
import org.specs.util._

/**
 * This trait models the structure of an Example with:<ul>
 * <li>subexamples</li>
 * <li>an example filter, which is a function filtering examples to execute</li>
 * </ul>
 */
trait ExampleStructure extends TreeNode with Tagged with DefaultResults { 
  
  /** number of <code>Expecatble</code> objects which refer to that Example */
  protected[specification] var thisExpectationsNumber = 0
  /** examples describing the sus behaviour */
  var exampleList = List[Example]()
  /** filters the examples which should be added to this Example structure */
  var examplesFilter: Example => Option[Example] = (e: Example) => Some(e)
  /** Declare the examples as components to be tagged when the sus is tagged */
  override def taggedComponents: List[Tagged] = this.exampleList

  /** add an example to the list of examples. */
  def addExample(e: Example) = {
    examplesFilter(e) map { ex =>
      addChild(ex)
      ex.accept(this.accepted:_*)
      ex.reject(this.rejected:_*)
      exampleList = exampleList ::: List(ex)
    }
  }
  /** create a new example with a description and add it to this. */
  def createExample(desc: String): Example
  /** @return this example if it doesn't have subexamples or return the subexamples */
  def allExamples: List[Examples]
  /** @return the total number of expectations for this sus */
  def ownExpectationsNb = { executeExamples; thisExpectationsNumber }
 /** @return true if there are failures or errors */
  def hasOwnFailureOrErrors = !(ownFailures ::: ownErrors).isEmpty
  /** @return the failures of this example, executing the example if necessary */
  def ownFailures: List[FailureException] = { executeExamples; thisFailures.toList }
  /** @return the skipped messages for this example, executing the example if necessary  */
  def ownSkipped: List[SkippedException] = { executeExamples; thisSkipped.toList }
  /** @return the errors of this example, executing the example if necessary  */
  def ownErrors: List[Throwable] = { executeExamples; thisErrors.toList }
  /** @return the failures of this example and its subexamples, executing the example if necessary */
  override def failures: List[FailureException] = { ownFailures ++ examples.flatMap { _.failures } }
  /** @return the skipped messages for this example and its subexamples, executing the example if necessary  */
  override def skipped: List[SkippedException] = { ownSkipped ++ examples.flatMap { _.skipped } }
  /** @return the errors of this example, executing the example if necessary  */
  override def errors: List[Throwable] = { ownErrors ++ examples.flatMap {_.errors} }
  /** @return true if there are only successes */
  def isFullSuccess = failures.isEmpty && skipped.isEmpty && errors.isEmpty
  /** @return all the examples with no errors, failures or skip messages */
  def successes = examples.filter(_.isFullSuccess)
  /** @return the number of expectations, executing the example if necessary */
  def expectationsNb: Int = ownExpectationsNb + examples.foldLeft(0)(_ + _.expectationsNb)
  /** return the list of examples contained in this one, possibly executing it */
  def examples = {
    executeExamples
    exampleList
  }
  /** execute the example, with possibly may create subexamples in order to be able to query them */
  def executeExamples() : Unit
  /** remove all failures and errors */
  def resetForExecution: this.type = {
    thisFailures.clear
    thisErrors.clear
    thisSkipped.clear
    this
  }
  /**
   * set the execution context for a cloned example: tags, filter, beforeFirst failure
   */
  private[specification] def prepareExecutionContextFrom(other: Examples) = {
    this.tagWith(other)
    this.examplesFilter = other.examplesFilter
  }
  /** 
   * copy the execution results from another example. This method is used to copy the results from another example
   * executed in isolation in another specification.
   */
  def copyFrom(other: ExampleStructure) = {
    examplesFilter = other.examplesFilter
    hardCopyResults(other)
    other.exampleList.foreach { e => 
      val ex = this.createExample(e.description.toString)
      ex.execution = e.execution
      ex.execution.map(_.example = ex)
      ex.execution.map(_.resetForExecution)
      ex.tagWith(e)
      ex.hasSomeSubExamples = e.hasSomeSubExamples
    }
    thisExpectationsNumber = other.thisExpectationsNumber
  }
}
