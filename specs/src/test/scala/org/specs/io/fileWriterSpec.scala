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
import org.specs._
import org.specs.runner._
import org.specs.io.mock._

class fileWriterSpec extends SpecificationWithJUnit {
  "A FileWriter" should {
    doAfter { new java.io.File("filePath").delete }
    "write inside a file" in {
      fw.write("filePath"){ file =>
        file.write("hello world")
      }
      out.messages mustContain "hello world"
    }
    "close the file if an exception occurs and rethrow the exception" in {
      try {
        fw.write("filePath"){ file =>
          throw new Error("bad")
        }
      } catch {case e => { e.getMessage mustBe "bad"}}
      out.closed mustBe true
    }
  }
  object fw extends FileWriter {
    override def getWriter(path: String) = out
  }
  object out extends MockWriter
}
