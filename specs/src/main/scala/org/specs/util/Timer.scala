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
package org.specs.util
import java.util.Calendar
import scala.collection.mutable.Stack
import org.specs.util.Plural._
/**
 * This trait provides Timer functionalities based on the Java Calendar milliseconds
 */
trait HmsTimer extends Timer {
  /** elapsed time since the last stop */
  var elapsed: Long = 0L

  /** current number of millis when instantiating the object using this Trait */
  var millis: Stack[Long] = new Stack[Long]

  /**
   * starts the with new elapsed time
   */
  def start = {
    elapsed = 0L
    millis.push(getTime)
  }

  /**
   * this method can be overriden for testing
   */
  protected def getTime = Calendar.getInstance.getTime.getTime

  /**
   * restarts the Timer with no elapsed time
   */
  def restart = {
    elapsed = 0L
    millis = new Stack[Long]
  }

  /**
   * Stop the timer, store the number of elapsed millis and return a String representing the time as hour/minute/second/ms
   * @return the elapsed time as a String
   */
  def stop: String = {
    val startMillis = if (!millis.isEmpty) millis.pop else 0L
    elapsed = getTime - startMillis
    preciseTime
  }

  /**
   * @return a tuple with the elapsed hours, minutes, seconds and millis
   */
  def hourMinutesSecondsMillis = {
    var totalMillis = elapsed
    val hours = totalMillis / 1000 / 3600
    totalMillis -= hours * 3600 * 1000
    val minutes = totalMillis / 1000 / 60
    totalMillis -= minutes * 60 * 1000
    val seconds = totalMillis / 1000
    val millis = totalMillis - seconds * 1000
    (hours, minutes, seconds, millis)
  }

  /**
   * @return a formatted string showing the hours, minutes and seconds
   */
  def hms: String = {
    val (hours, minutes, seconds, millis) = hourMinutesSecondsMillis
    var result = ""
    if (hours > 0) { result += hours + " hour".plural(hours) + " " }
    if (minutes > 0) { result += minutes + " minute".plural(minutes) + " " }
    result += (seconds + " second".plural(seconds))
    result
  }

  /**
   * @return a formatted string showing the hours, minutes, seconds and millis
   */
  def preciseTime: String = {
    val (hours, minutes, seconds, millis) = hourMinutesSecondsMillis
    hms + ", " + millis + " ms"
  }
  def time = preciseTime
}

/**
 * The Timer trait acts as a simple stopwatch. It can be stopped to get the elapsed time as a formatted String.
 */
trait Timer {
  def stop: String
  def hms: String
  def start
  def restart
  def time: String
}

/**
 * Default class for the HmsTimer trait
 */
class SimpleTimer extends HmsTimer

