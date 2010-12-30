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
import java.util.concurrent.TimeUnit._

/** 
 * The WaitFor trait can be used to add some Thread.sleep instructions to an executed specification.<p>
 * Usage:<pre>
 * waitFor(10.ms) // wait for 10 milliseconds
 * </pre>
 */
trait WaitFor {
  /** 
   * wait for a given time specified by the delay
   */
  def waitFor(delay: Delay): Unit = waitFor(delay.millis)

  /** 
   * wait for a given number of milliseconds
   */
  def waitFor(i: Int): Unit = Thread.sleep(i)

  /** 
   * @return a Delay object The WaitFor trait can be used to add some Thread.sleep instructions to an executed specification
   */
  implicit def intToDelay(i: Int) = Delay(i)

  /** 
   * A Delay represents a time interval expressed with a TimeUnit in nanoseconds, microseconds, milliseconds or seconds.
   * Note that the symbol for micro is us with the u approximating the Greek symbol "mu"
   */
  case class Delay(i: Int) {
    var timeUnit = MILLISECONDS
    def ns = { timeUnit = NANOSECONDS; this }
    def us = { timeUnit = MICROSECONDS; this }
    def ms = { timeUnit = MILLISECONDS; this }
    def s = { timeUnit = SECONDS; this }
    
    /** 
     * @return the delay in nanos
     */
    def nanos = {
      if (timeUnit == NANOSECONDS) 
        i
      else if (timeUnit == MICROSECONDS)
        i * 1000
      else if (timeUnit == MILLISECONDS)
        i * 1000000
      else
        i * 1000000000
    }

    /** 
     * @return the delay in micros
     */
    def micros = {
      if (timeUnit == NANOSECONDS) 
        i / 1000
      else if (timeUnit == MICROSECONDS)
        i
      else if (timeUnit == MILLISECONDS)
        i * 1000
      else
        i * 1000000
    }

    /** 
     * @return the delay in millis
     */
    def millis = {
      if (timeUnit == NANOSECONDS) 
        i / 1000000
      else if (timeUnit == MICROSECONDS)
        i / 1000
      else if (timeUnit == MILLISECONDS)
        i
      else
        i * 1000
    }

    /** 
     * @return the delay in seconds
     */
    def seconds = {
      if (timeUnit == NANOSECONDS) 
        i / 1000000000
      else if (timeUnit == MICROSECONDS)
        i / 1000000
      else if (timeUnit == MILLISECONDS)
        i / 1000
      else
        i
    }
  }
}
