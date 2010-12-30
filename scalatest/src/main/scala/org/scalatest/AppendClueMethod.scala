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
 * Trait implemented by exception types that can append a new clue to the end
 * of the exception's detail message.
 *
 * <p>
 * This trait facilitates the <code>withClue</code> construct provided by trait
 * <code>Assertions</code>. This construct enables extra information (or "clues") to
 * be included in the detail message of a thrown exception. Although both
 * <code>assert</code> and <code>expect</code> provide a way for a clue to be
 * included directly, <code>intercept</code> and ScalaTest matcher expressions
 * do not. Here's an example of clues provided directly in <code>assert</code>:
 * </p>
 *
 * <pre>
 * assert(1 + 1 === 3, "this is a clue")
 * </pre>
 *
 * <p>
 * and in <code>expect</code>:
 * </p>
 *
 * <pre>
 * expect(3, "this is a clue") { 1 + 1 }
 * </pre>
 *
 * <p>
 * The exceptions thrown by the previous two statements will include the clue
 * string, <code>"this is a clue"</code>, in the exceptions detail message.
 * To get the same clue in the detail message of an exception thrown
 * by a failed <code>intercept</code> call requires using <code>withClue</code>:
 * </p>
 *
 * <pre>
 * withClue("this is a clue") {
 *   intercept[IndexOutOfBoundsException] {
 *     "hi".charAt(-1)
 *   }
 * }
 * </pre>
 *
 * <p>
 * Similarly, to get a clue in the exception resulting from an exception arising out
 * of a ScalaTest matcher expression, you need to use <code>withClue</code>. Here's
 * an example:
 * </p>
 *
 * <pre>
 * withClue("this is a clue") {
 *   1 + 1 should be === 3
 * }
 * </pre>
 *
 * <p>
 * Exception types that mix in this trait have an <code>appendClue</code> method, which
 * returns an exception identical to itself, except with the result of invoking
 * <code>toString</code> on the passed <code>clue</code> appended to the exception's
 * detail message, separated by a newline character (or replacing the detail message
 * if it was previously <code>null</code>).   
 * </p>
 */
private[scalatest] trait AppendClueMethod[T <: Throwable] { this: Throwable =>

  /**
   * Returns an instance of this exception's class, identical to this exception,
   * except with the string resulting from invoking <code>toString</code> on
   * the passed <code>clue</code> appended to its detail message, separated by
   * a newline character. Or, if this exception's detail message is <code>null</code>,
   * returns an instance of this exception's class that is identical to this exception,
   * except with the detail message equal to the result of invoking <code>toString</code>
   * on the passed <code>clue</code>.
   *
   * <p>
   * Implementations of this method may either mutate this exception or return
   * a new instance with the revised detail message. If the result of invoking
   * <code>toString</code> on the passed <code>clue</code> is null or, after being
   * trimmed, an empty string, this method returns this exception unchanged.
   * </p>
   *
   * @param clue An object whose <code>toString</code> method returns a message to append
   *   to this exception's detail message.
   */
  def appendClue(clue: Any): T
}
