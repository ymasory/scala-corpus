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
 * Class formerly used to send specification-style reports to a <code>Reporter</code>.  <strong>Note: This class has been deprecated and
 * will be removed in a future version of ScalaTest.</strong>
 * </p>
 *
 * <p>
 * <strong>As of version 1.0, class <code>SpecReport</code> is no longer used by the ScalaTest API. It has essentially been replaced
 * by the event mechanism in package <code>org.scalatest.events</code>. It will be removed after a two-release deprecation cycle.
 * Please migrate any uses of <code>SpecReport</code> to use the new event mechanism.</strong>
 * </p>
 *
 * @author Bill Venners
 */
@deprecated
@serializable
class SpecReport(
  override val name: String,
  override val message: String,
  val plainSpecText: String,
  val formattedSpecText: String,
  val includeInSpecOutput: Boolean,
  override val throwable: Option[Throwable],
  override val rerunnable: Option[Rerunnable],
  override val threadName: String,
  override val date: Date
) extends Report(name, message, throwable, rerunnable, threadName, date) {

  if (plainSpecText == null)
    throw new NullPointerException("plainSpecText was null")
  if (formattedSpecText == null)
    throw new NullPointerException("formattedSpecText was null")

  /**
   * <strong>Note: This class has been deprecated and will be removed in a future version of ScalaTest.</strong>
   */
  @deprecated
  def this(name: String, message: String, plainSpecText: String, formattedSpecText: String, includeInSpecOutput: Boolean) =
    this(name, message, plainSpecText, formattedSpecText, includeInSpecOutput, None, None, Thread.currentThread.getName, new Date)

  /**
   * <strong>Note: This class has been deprecated and will be removed in a future version of ScalaTest.</strong>
   */
  @deprecated
  def this(name: String, message: String, plainSpecText: String, formattedSpecText: String, includeInSpecOutput: Boolean, throwable: Option[Throwable], rerunnable: Option[Rerunnable])  = this(name,
      message, plainSpecText, formattedSpecText, includeInSpecOutput, throwable, rerunnable, Thread.currentThread.getName, new Date)
}
