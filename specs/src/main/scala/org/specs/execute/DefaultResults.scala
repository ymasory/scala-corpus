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
package org.specs.execute
import scala.collection.mutable.ListBuffer

/**
 * Default implementation for the HasResults trait using lists.
 */
trait DefaultResults extends HasResults {
  protected val thisFailures: ListBuffer[FailureException] = new ListBuffer
  protected val thisErrors: ListBuffer[Throwable] = new ListBuffer
  protected val thisSkipped: ListBuffer[SkippedException] = new ListBuffer
  
  /**
   * reset the results to no issues.
   */
  def reset(): this.type = { 
    thisFailures.clear
    thisErrors.clear
    thisSkipped.clear 
    this
  }
  /** add a new failure */
  def addFailure(f: FailureException): this.type = { thisFailures.append(f); this }
  /** add a new error */
  def addError(t: Throwable): this.type = { thisErrors.append(t); this }
  /** add a new skipped */
  def addSkipped(s: SkippedException): this.type = { thisSkipped.append(s); this }
  /** @return the list of failures */
  def failures: List[FailureException] = thisFailures.toList
  /** @return the list of errors */
  def errors: List[Throwable] = thisErrors.toList
  /** @return the list of skipped */
  def skipped: List[SkippedException] = thisSkipped.toList
  /** copy the results of another HasResult object */
  override def copyResults(other: HasResults): this.type = {
    reset()
    other.failures.foreach(addFailure(_))
    other.errors.foreach(addError(_))
    other.skipped.foreach(addSkipped(_))
    this
  }
  def hardCopyResults(other: DefaultResults): this.type = {
    reset()
    other.thisFailures.foreach(addFailure(_))
    other.thisErrors.foreach(addError(_))
    other.thisSkipped.foreach(addSkipped(_))
    this
  }
}
