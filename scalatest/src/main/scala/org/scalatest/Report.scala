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

import java.util.Date

/**
 * <p>
 * Class formerly used to send reports to a <code>Reporter</code>. <strong>Note: This class has been deprecated and
 * will be removed in a future version of ScalaTest.</strong>
 * </p>
 *
 * <p>
 * <strong>As of version 1.0, class <code>Report</code> is no longer used by the ScalaTest API. It has essentially been replaced
 * by the event mechanism in package <code>org.scalatest.events</code>. It will be removed after a two-release deprecation cycle.
 * Please migrate any uses of <code>Report</code> to use the new event mechanism.</strong>
 * </p>
 *
 * @author Bill Venners
 */
@deprecated
@serializable
class Report(val name: String, val message: String, val throwable: Option[Throwable], val rerunnable: Option[Rerunnable],
    val threadName: String, val date: Date) {

  if (name == null)
    throw new NullPointerException("name was null")

  if (message == null)
    throw new NullPointerException("message was null")

  if (threadName == null)
    throw new NullPointerException("thread was null")
  
  if (date == null)
    throw new NullPointerException("date was null")
  
  if (throwable == null)
    throw new NullPointerException("throwable was null")
  
  if (rerunnable == null)
    throw new NullPointerException("rerunnable was null")

  /**
   * <strong>Note: This class has been deprecated and will be removed in a future version of ScalaTest.</strong>
   */
  @deprecated
  def this(name: String, message: String) = this(name, message,
      None, None, Thread.currentThread.getName, new Date)

// def this(name: String, message: String, rerunnable: Option[Rerunnable]) = this(name,
//    message, None, rerunnable, Thread.currentThread.getName, new Date)
// [bv: this will trip people up. Option's type is erased, so overloading this way didn't work. So
// may want to mention this somewhere]
// I realized that it would be dumb to use this form if you didn't have a rerunable, so it shouldn't be an Option anyway.
// It still may be worth mentioning, because this error will probably happen to every newbie at some point.

  /**
   * <strong>Note: This class has been deprecated and will be removed in a future version of ScalaTest.</strong>
   */
  @deprecated
  def this(name: String, message: String, throwable: Option[Throwable], rerunnable: Option[Rerunnable])  = this(name,
      message, throwable, rerunnable, Thread.currentThread.getName, new Date)
}
/*
This was an interesting excercise to decide whether to provide overloaded constructors
for say:

1. String, String, Option[Throwable]

2. String, String, Throwable, Rerunnable

3. String String, Throwable

4. String, String, Rerunable

Decided to make people say None at the end, so no 1. Decided it was confusing to have throwable be
both an Option[Throwable] and a Throwable. So didn't do the others. Maybe convenient, but confusing
and i realized sometimes I had an Option[Rerunnable] variable already, so I really did want to pass
that in without checking it with a match. (for a while I thought I would only have 2, 3, and 4.)
*/
