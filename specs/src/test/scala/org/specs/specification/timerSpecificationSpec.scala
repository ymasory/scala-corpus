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
import org.specs._
import org.specs.specification._
import org.specs.runner._
import org.specs.util._
import scala.xml._
import org.specs.Sugar._

class timerSpecificationSpec extends TimerSpecificationActionWords {
  "The timer specification" is <p>
   A Simple timer is an object which can measure time. Let's create a timer.
   When a timer is stopped{stop}, the timer should {"fail to return the elapsed time" in failTime} then
   {"return the elapsed time" in succeeds}

   A person can have its name reset. If the person's name is set to {"Peter" as personName},
   then {"the person must be named Peter" in checkName}
</p>
}

class TimerSpecificationActionWords extends HtmlSpecificationWithJUnit {
  val simpleTimer = new SimpleTimer
  class Person {var name: String = ""; def setName(n: String) = name = n}
  val person = new Person;
  val Peter = "Peter"
  def stop = simpleTimer.stop.shh
  def failTime = simpleTimer.hms must beMatching("\\d second")
  def succeeds = simpleTimer.hms must beMatching("\\d second")
  def personName = person.setName _
  def checkName = person.name must_== Peter
}

