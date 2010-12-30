/*
 * Copyright 2001-2008 Artima, Inc.
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
 * Exception that indicates a test failed. The purpose of this exception is to encapsulate information about
 * the stack depth at which the line of test code that failed resides, so that information can be presented to
 * the user that makes it quick to find the failing line of test code. (In other words, the user need not scan through the
 * stack trace to find the correct filename and line number of the failing test.)
 *
 * @param message an optional detail message for this <code>TestFailedException</code>.
 * @param cause an optional cause, the <code>Throwable</code> that caused this <code>TestFailedException</code> to be thrown.
 * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
 *
 * @throws NullPointerException if <code>message</code> is <code>null</code>, or <code>Some(null)</code>.
 * @throws NullPointerException if <code>cause</code> is <code>null</code>, or <code>Some(null)</code>.
 *
 * @author Bill Venners
 */
class TestFailedException(message: Option[String], cause: Option[Throwable], failedCodeStackDepth: Int)
    extends StackDepthException(message, cause, failedCodeStackDepth) {
  
  if (message == null) throw new NullPointerException("message was null")
  message match {
    case Some(null) => throw new NullPointerException("message was a Some(null)")
    case _ =>
  }

  if (cause == null) throw new NullPointerException("cause was null")
  cause match {
    case Some(null) => throw new NullPointerException("cause was a Some(null)")
    case _ =>
  }

  /**
   * <strong><code>failedTestCodeStackDepth</code> has been deprecated and will be removed in a future version of
   * ScalaTest. Please call <code>failedCodeStackDepth</code> instead.</strong>
   */
  @deprecated // deprecated in 1.0, remove in 1.2
  val failedTestCodeStackDepth: Int = failedCodeStackDepth

  /**
   * <strong><code>failedTestCodeFileNameAndLineNumberString</code> has been deprecated and will be removed in a future version of
   * ScalaTest. Please call <code>failedCodeFileNameAndLineNumberString</code> instead.</strong>
   */
   @deprecated // deprecated in 1.0, remove in 1.2
   val failedTestCodeFileNameAndLineNumberString: Option[String] = failedCodeFileNameAndLineNumberString

  /**
   * Create a <code>TestFailedException</code> with specified stack depth and no detail message or cause.
   *
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   */
  def this(failedCodeStackDepth: Int) = this(None, None, failedCodeStackDepth)

  /**
   * Create a <code>TestFailedException</code> with a specified stack depth and detail message.
   *
   * @param message A detail message for this <code>TestFailedException</code>.
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
  def this(message: String, failedCodeStackDepth: Int) =
    this(
      {
        if (message == null) throw new NullPointerException("message was null")
        Some(message)
      },
      None,
      failedCodeStackDepth
    )

  /**
   * Create a <code>TestFailedException</code> with the specified stack depth and cause.  The
   * <code>message</code> field of this exception object will be initialized to
   * <code>if (cause.getMessage == null) "" else cause.getMessage</code>.
   *
   * @param cause the cause, the <code>Throwable</code> that caused this <code>TestFailedException</code> to be thrown.
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullPointerException if <code>cause</code> is <code>null</code>.
   */
  def this(cause: Throwable, failedCodeStackDepth: Int) =
    this(
      {
        if (cause == null) throw new NullPointerException("cause was null")
        Some(if (cause.getMessage == null) "" else cause.getMessage)
      },
      Some(cause),
      failedCodeStackDepth
    )

  /**
   * Create a <code>TestFailedException</code> with the specified stack depth, detail
   * message, and cause.
   *
   * <p>Note that the detail message associated with cause is
   * <em>not</em> automatically incorporated in this throwable's detail
   * message.
   *
   * @param message A detail message for this <code>TestFailedException</code>.
   * @param cause the cause, the <code>Throwable</code> that caused this <code>TestFailedException</code> to be thrown.
   * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
   *
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   * @throws NullPointerException if <code>cause</code> is <code>null</code>.
   */
  def this(message: String, cause: Throwable, failedCodeStackDepth: Int) =
    this(
      {
        if (message == null) throw new NullPointerException("message was null")
        Some(message)
      },
      {
        if (cause == null) throw new NullPointerException("cause was null")
        Some(cause)
      },
      failedCodeStackDepth
    )
}

