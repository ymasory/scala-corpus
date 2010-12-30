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

/** Anything that's executable and returns itself */
trait Executable {
  def execute: this.type
}

/** 
 * This trait represents something that's executable and has results.
 * 
 * It can also be commented and not be executed in that case.
 * 
 * Subclasses can access the execution status with the "executionStarted" and "executed" variables.
 */
trait DefaultExecutable extends DefaultResults with Executable with Commentable {
  protected var executionStarted = false
  protected var executed = false

  /** reset the execution to "not executed". This also resets the results. */
  override def reset(): this.type = {
    super[DefaultResults].reset()
    executed = false
    executionStarted = false
    this
  }
  
  /**
   * Extending classes must define this method to define the executed behavior 
   */
  protected def executeThis: Any
  
  /**
   * Execute this only if it is not commented.
   * 
   * The execution starts by a reset(), then catches and FailureException, SkippedException or Throwables to store results
   */
  def execute: this.type = {
    reset()
    if (!isCommented) {
      try {
        executionStarted = true
        executeThis
      } catch {
        case f: FailureException => addFailure(f)
        case s: SkippedException => addSkipped(s)
        case e => addError(e)
      } finally { 
        executed = true
        executionStarted = false
      }
    }
    this
  }
}