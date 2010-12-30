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
 * Exception that indicates an action that is only allowed during a suite's test registration phase,
 * such as registering a test to run or ignore, was attempted after registration had already closed.
 *
 * <p>
 * In suites that register tests as functions, such as <code>FunSuite</code> and <code>Spec</code>, tests
 * are normally registered during construction. Although it is not the usual approach, tests can also
 * be registered after construction by invoking methods that register tests on the already constructed suite so
 * long as <code>run</code> has not been invoked on that suite.
 * As soon as <code>run</code> is invoked for the first time, registration of tests is "closed," meaning
 * that any further attempts to register a test will fail (and result in an instance of this exception class being thrown). This
 * can happen, for example, if an attempt is made to nest tests, such as in a <code>FunSuite</code>:
 * </p>
 *
 * <pre>
 * test("this test is fine") {
 *   test("but this nested test is not allowed") {
 *   }
 * }
 * </pre>
 *
 * <p>
 * This exception encapsulates information about the stack depth at which the line of code that made this attempt resides,
 * so that information can be presented to the user that makes it quick to find the problem line of code. (In other words,
 * the user need not scan through the stack trace to find the correct filename and line number of the offending code.)
 * </p>
 *
 * @param testName the test name that was attempted to be registered after registration had closed
 * @param failedCodeStackDepth the depth in the stack trace of this exception at which the line of code that attempted
 *   to register the test after registration had been closed.
 *
 * @throws NullPointerException if <code>testName</code> is <code>null</code>
 *
 * @author Bill Venners
 */
class TestRegistrationClosedException(message: String, failedCodeStackDepth: Int)
    extends StackDepthException(Some(message), None, failedCodeStackDepth)

// I pass in a message here so different situations can be described better in the
// error message, such as an it inside an it, an ignore inside an it, a describe inside an it, etc.
