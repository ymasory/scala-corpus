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
 * Exception that indicates something was attempted in test code that is not allowed.
 * For example, in a <code>FeatureSpec</code>, it is not allowed to nest a <code>feature</code>
 * clause inside another <code>feature</code> clause. If this is attempted, the construction
 * of that suite will fail with a <code>NotAllowedException</code>.
 *
 * @param message a string that explains the problem
 * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of code that attempted
 *   to register the test with the duplicate name resides.
 *
 * @throws NullPointerException if <code>message</code> is <code>null</code>
 *
 * @author Bill Venners
 */
class NotAllowedException(message: String, failedCodeStackDepth: Int)
    extends StackDepthException(Some(message), None, failedCodeStackDepth) {

  if (message == null)
    throw new NullPointerException("message was null")
}