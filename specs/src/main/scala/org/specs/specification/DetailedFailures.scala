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
package org.specs.specification
import org.specs.util.Configuration
/**
 * This traits adds the possibility to add declarations for detailed failures when matching strings.<br/>
 * Generally detailed failures can be configured with:<ul>
 * <li>the expected separators: [] are the default</li>
 * <li>a trigger size: the minimal size of the string to match for which to show detailed failures</li>
 * <li>a shorten size: the size of text which should show up between 2 differences, the rest being elided</li>
 * </ul>
 */
trait DetailedFailures {
  /** by default no full details are reported by specifications */
  implicit var detailedFailures: Detailed = if (Configuration.config.smartDiffs) new fullDetails("[]", 30, 20) else noDetails()
  /** detailled diffs enable showing the differences when comparing the toString result of 2 objects supposed to be == */
  def detailedDiffs(): Unit = detailedDiffs("[]", 0, 20)
  /** detailled diffs enable showing the differences when comparing the toString result of 2 objects supposed to be == */
  def detailedDiffs(separators: String): Unit = detailedDiffs(separators, 0, 20)
  /** detailled diffs enable showing the differences when comparing the toString result of 2 objects supposed to be == */
  def detailedDiffs(separators: String, shortenSize: Int): Unit = detailedDiffs(separators, 0, shortenSize)
  /** detailled diffs enable showing the differences when comparing the toString result of 2 objects supposed to be == */
  def detailedDiffs(separators: String, startDiffSize: Int, shortenSize: Int): Unit = { detailedFailures = new fullDetails(separators, startDiffSize, shortenSize) }
  /** reset the detailled diffs to no diffs */
  def noDetailedDiffs() = { detailedFailures = noDetails() }
}
/** abstract data type representing Detailed information about failures */
abstract class Detailed
/** no details should be shown */
case class noDetails() extends Detailed
/** all details should be shown */
case class fullDetails(separators: String, startDiffSize: Int, shortenSize: Int) extends Detailed {
  def this(sep: String) = this(sep, 30, 20)
  def this() = this("[]") 
}
