/*
 * Copyright 2001-2009 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest

/**
 * Trait that encapsulates the information required of an exception thrown by ScalaTest's assertions
 * and matchers, which includes a stack depth at which the failing line of test code resides.
 *
 * <p>
 * This trait exists so that it can be mixed into two exception superclasses, <code>StackDepthException</code>,
 * from which extend several exceptions that do not depend on JUnit, and <code>JUnitTestFailedError</code>, which
 * does depend on JUnit. The latter, which requires JUnit be in the classpath, ensures failed ScalaTest assertions are
 * reported as "failures," not "errors," by JUnit.
 * </p>
 */
trait StackDepth { this: Throwable =>

  /**
   * An optional detail message for this <code>StackDepth</code> exception.
   */
  val message: Option[String]
 
  /**
   * An optional cause, the <code>Throwable</code> that caused this <code>StackDepth</code> exception to be thrown.
   */
  val cause: Option[Throwable]

  /**
   * The depth in the stack trace of this exception at which the line of test code that failed resides.
   */
  val failedCodeStackDepth: Int

  /**
   * A string that provides the filename and line number of the line of code that failed, suitable
   * for presenting to a user, which is taken from this exception's <code>StackTraceElement</code> at the depth specified
   * by <code>failedCodeStackDepth</code>.
   *
   * @return a user-presentable string containing the filename and line number that caused the failed test
   */
  val failedCodeFileNameAndLineNumberString: Option[String] = {
    val stackTraceElement = getStackTrace()(failedCodeStackDepth)
    val fileName = stackTraceElement.getFileName
    if (fileName != null) {
      Some(fileName + ":" + stackTraceElement.getLineNumber)
    }
    else None
  }
}
