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
package org.scalatest.prop

import org.scalatest._

/**
 * Exception that indicates a test failed. The purpose of this exception is to encapsulate information about
 * the stack depth at which the line of test code that failed resides, so that information can be presented to
 * the user that makes it quick to find the failing line of test code. (I.e., the user need not scan through the
 * stack trace to find the correct filename and line number of the failing test.)
 *
 * @param message an optional detail message for this <code>TestFailedException</code>.
 * @param cause an optional cause, the <code>Throwable</code> that caused this <code>TestFailedException</code> to be thrown.
 * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of test code that failed resides.
 * @param undecoratedMessage just a short message that has no redundancy with args, labels, etc. The regular "message" has everything in it
 * @param args the argument values, if any
 *
 * @throws NullPointerException if <code>message</code> is <code>null</code>, or <code>Some(null)</code>.
 * @throws NullPointerException if <code>cause</code> is <code>null</code>, or <code>Some(null)</code>.
 *
 * @author Bill Venners
 */
class PropertyTestFailedException(
  message: String,
  cause: Option[Throwable],
  failedCodeStackDepth: Int,
  val undecoratedMessage: String,
  val args: List[Any],
  val labels: List[String]
) extends TestFailedException(Some(message), cause, failedCodeStackDepth) {

  if (message == null) throw new NullPointerException("message was null")

  if (cause == null) throw new NullPointerException("cause was null")
  cause match {
    case Some(null) => throw new NullPointerException("cause was a Some(null)")
    case _ =>
  }
}

