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
package org.specs.runner

import org.specs._
import org.specs.specification._

/**
 * This class can be used to search for specifications on a given path
 * and execute them.<br>
 * Usage: <code>object myFileRunner extends SpecsFileRunner(path, pattern)</code><br>
 * Where <code>path</code> is a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
 * and <code>pattern</code> is a regular expression which is supposed to match an object name extending a Specification
 * class named ".*Spec.*"
 *
 * The systems and examples can also be filtered by specifying patterns. These patterns can be passed as System properties,
 * by specifying -Dsus="regexp" or -Dexample="regexp".
 */
class SpecsFileRunner(path: String, val specFilterPattern: String, filterForSus: String, filterForExample: String) extends
  SpecsFinder(path, specFilterPattern, true) with Console {

  /** define the sus pattern on the SpecsFilter trait. */
  override def susFilterPattern = filterForSus

  /** define the example pattern on the SpecsFilter trait. */
  override def exampleFilterPattern = filterForExample

  /** short constructor with no filter for sus or examples. */
  def this(path: String, specFilterPattern: String) = this(path, specFilterPattern, ".*", ".*")

  /** short constructor with the path only. */
  def this(path: String) = this(path, ".*", ".*", ".*")
}
