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
package org.specs.matcher
import org.specs.util.Duration
import org.specs.util.TimeConversions._

/**
 * This trait provides functions to delay the evaluation of a matcher.
 * 
 * This matcher will be retried a given number of times, using a given sleeping time in between
 */
trait EventuallyMatchers {
  
  /**
   * @return a matcher that will retry the nested matcher a given number of times
   */
  def eventually[T](retries: Int, sleep: Duration)(nested: Matcher[T]): Matcher[T] = new Matcher[T]() {
    def apply(a: => T) = retry(retries, sleep, a)

    def retry(retries: Int, sleep: Duration, a: => T): (Boolean, String, String) = {
      val result = nested(a)
      if (result.success || retries == 1) {
        result
      } else {
        Thread.sleep(sleep.inMillis)
        retry(retries - 1, sleep, a)
      }
    }
  }

  /**
   * @return a matcher that will retry the nested matcher a given 40 times
   */
  def eventually[T](nested: Matcher[T]): Matcher[T] = eventually(40, 100.milliseconds)(nested)
}
object EventuallyMatchers extends EventuallyMatchers 