/*
 * Copyright 2009 Twitter, Inc.
 * Copyright 2009 Robey Pointer <robeypointer@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.lag.smile.stress

import _root_.net.lag.configgy.Configgy
import _root_.net.lag.extensions._
import _root_.net.lag.logging.{Level, Logger}
import _root_.java.util.Random


trait StressTest {
  val rnd = new Random(System.currentTimeMillis)
  var hosts: Array[String] = _

  def setHosts(newHosts: Array[String]) {
    hosts = newHosts
  }

  def report(name: String, count: Int)(f: => Unit): Unit = {
    val start = System.currentTimeMillis
    f
    val duration = System.currentTimeMillis - start
    val average = duration * 1.0 / count
    println("%s: %d in %d msec (%.2f msec each)".format(name, count, duration, average))
  }

  def generateValue(size: Int): String = {
    val sb = new StringBuffer(size)
    for (idx <- 1 to size) sb.append(('a' + rnd.nextInt('z' - 'a')).asInstanceOf[Char])
    sb.toString()
  }

  def generateValues(size: Int, variance: Int, count: Int): List[String] = {
    (for (i <- 1 to count) yield generateValue(size + rnd.nextInt(variance))).toList
  }
}
