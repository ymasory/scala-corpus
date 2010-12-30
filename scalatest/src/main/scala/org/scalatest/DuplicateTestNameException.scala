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
 * Exception that indicates an attempt was made to register a test that had the same name as a test
 * already registered in the same suite. The purpose of this exception is to encapsulate information about
 * the stack depth at which the line of code that made this attempt resides, so that information can be presented to
 * the user that makes it quick to find the problem line of code. (In other words, the user need not scan through the
 * stack trace to find the correct filename and line number of the offending code.)
 *
 * @param testName the test name that was attempted to be registered twice
 * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of code that attempted
 *   to register the test with the duplicate name resides.
 *
 * @throws NullPointerException if <code>testName</code> is <code>null</code>
 *
 * @author Bill Venners
 */
class DuplicateTestNameException(testName: String, failedCodeStackDepth: Int)
    extends StackDepthException(Some(Resources("duplicateTestName", testName)), None, failedCodeStackDepth) {
  
  if (testName == null)
    throw new NullPointerException("testName was null")
}

