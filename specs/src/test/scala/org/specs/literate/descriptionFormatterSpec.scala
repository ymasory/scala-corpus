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
import scala.xml._
import org.specs.Sugar._
import org.specs.specification._
import org.specs.runner._
import org.specs._

class descriptionFormatterSpec extends SpecificationWithJUnit {

  "A text formatter" should {
    "format a description as text" in {
      textFormatter.format(<t>Hello world</t>).text must_== "Hello world"
    }
  }
  "A Textile formatter" should {
    "format a description as textile markup" in {
      textileFormatter.format(<t>h1. Hello world</t>) must \\("h1")
    }
    "set the status of the example descriptions depending on the example status" in {
      val example = "example desc" in { 1 must_== 1 }
      example.executeThis
      textileFormatter.setStatus("this is the " + example.description + " to be highlighted", List(example)).toString must 
        include("""this is the ==<ex class="success" """) and
        include("</ex>== to be highlighted")
    }
  }
  "An html formatter" should {
    "format a description as html" in {
      htmlFormatter.format(<t>This is some <i>html</i> text</t>) must \\("i")
    }
  }
  def textFormatter = new TextFormatter
  def textileFormatter = new TextileFormatter
  def htmlFormatter = new HtmlFormatter
}
