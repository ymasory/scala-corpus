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
package org.specs.literate
import org.specs.specification.Example
import scala.xml._

class markdownFormatterSpec extends org.spex.Specification {
  def formatString(s: String): String = new MarkdownFormatter{}.format(s)
  def formatElem(e: Elem): Node = new MarkdownFormatter{}.format(e)

  "A markdown formatter" should {
    "return a string as it is if isn't some html text" in {
      formatString("a description") must_== "a description"
    }
    "format the description of example as some xml text" in {
      val example = new Example("", null)
      example.exampleDescription = new Markdown(){}.makeExampleDescription(<ex>a description</ex>)
      new MarkdownFormatter().formatDesc(example) must_== <t>a description</t>
    }
    "format single quotes as single quotes" in {
      formatString("don't") must include("don't")
    }
    "format html which is well formed for further parsing" in {
      formatElem(<ex>this is some *text* to format</ex>) must ==/(<div><p>this is some <em>text</em> to format</p></div>)
    }
  }
}
