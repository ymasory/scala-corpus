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
package org.specs.io;
import java.io._

/**
 * The FileWriter trait provides functions to write files
 */
trait FileWriter {

  /**
   * writes some content to a file and take care of closing the file.<p>
   * Usage: <pre>
   * write("./dir/hello.txt") { out =>
   *   out.write("content")
   * }
   * </pre>
   * @param path path of the file to write
   */
  def write(path: String)(function: Writer => Unit): Unit = {
    createFile(path)
    val out = getWriter(path)
    try {
      function(out)
    } finally {
      try {
      out.close()
      } catch { case _ => }
    }
  }
  /**
   * creates a file for a given path. Create the parent directory if necessary.
   */
  def createFile(path: String) = {
    if (new File(path).getParentFile != null && !new File(path).getParentFile.exists) 
      mkdirs(new File(path).getParent) 
    if (!new File(path).exists) 
      new File(path).createNewFile
  }

  /** creates a new directory */
  def mkdirs(path: String) = new File(path).mkdirs
  /**
   * writes some content to a file.
   * @param path path of the file to read
   * @content content of the file to write
   */
  def writeFile(path: String, content: =>String): Unit = write(path) { out => out.write(content) }

  /**
   * The getWriter function can be overriden to provide a mock writer writing to the console for example
   * @return a Writer object opened on the file designated by <code>path</code>
   */
  def getWriter(path: String): Writer = new BufferedWriter(new java.io.FileWriter(path))
}
