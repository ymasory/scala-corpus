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
package org.specs.io

/**
 * Abstract trait representing an output with standard print functions
 */
trait Output {
  /**
   * prints an object with a newline
   */
  def println(m: Any)

  /**
   * prints several objects according to a format string (see Console.printf)
   */
  def printf(format: String, args: Any*)

  /**
   * flushes the content if necessary
   */
  def flush() = {}

  /**
   * prints stacktraces
   */
  def printStackTrace(t: Throwable) = t.printStackTrace(new java.io.PrintWriter(System.err) {
    override def println(s: String) = Output.this.println(s)
  })
}

/**
 * Implementation of the <code>Output</code> trait using the Console object
 */
trait ConsoleOutput extends Output {
  /**
   * prints an object with a newline
   */
  def println(m: Any) = Console.println(m)
  
  /**
   * prints several objects according to a format string (see Console.printf)
   */
  def printf(format: String, args: Any*) =  Console.printf(format, args: _*)
  
  /**
   * flushes the content if necessary
   */
  override def flush() = Console.flush()
}
