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

/**
 * This trait is useful to get a common interface for anything holding results made of 
 * failures, errors and skipped anything, like Specifications, Sus and Examples.
 * 
 * This trait is also implemented by the DefaultResults trait using lists to store values.
 * 
 * Failures are modeled by FailureExceptions
 * Errors are modeled by Throwables
 * Skipped (meaning "not executed") are modeled by SkippedExceptions
 */
trait HasResults {
  /** @return a list of failures */
  def failures: Seq[FailureException]
  /** @return a list of skipped exceptions (whose messages include the reason for skipping) */
  def skipped: Seq[SkippedException]
  /** @return a list of errors */
  def errors: Seq[Throwable]
  /** @return a string showing the status. "error" if there are errors, then "failure", "skipped" or finally "success" */
  def statusClass = {
    if (!errors.isEmpty)
      "error"
    else if (!failures.isEmpty)
      "failure"
    else if (!skipped.isEmpty)
      "skipped"
    else
      "success"
  }
  /** @return the status as a text icon. x for an issue, o for a skip, + for a success */
  def statusAsText = {
    if (!failureAndErrors.isEmpty)
      "x"
    else if (!skipped.isEmpty)
      "o"
    else
      "+"
  }
  /** @return true if there are failures or errors */
  def hasFailureOrErrors = !failureAndErrors.isEmpty
  /** @return failures and errors */
  def failureAndErrors = (failures ++ errors).toList
  /** @return issues = anything that's not a success */
  def issues = (failures ++ errors ++ skipped).toList
  /** @return issues as a list of messages, comma-separated */
  def issueMessages = issues.map(_.getMessage).mkString(", ")
  /** @return true if there are issues */
  def hasIssues = !issues.isEmpty
  /** @return true if there are no issues */
  def isOk = issues.isEmpty
  /** copy the results of another HasResult object */
  def copyResults(other: HasResults): this.type = this
}
object Status extends Enumeration("success", "failure", "error", "skipped", "info") {
  type Status = Value
  val Success, Failure, Error, Skipped, Info = Value
}

