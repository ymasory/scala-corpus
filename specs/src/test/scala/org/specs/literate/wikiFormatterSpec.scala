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
import org.specs._
import org.specs.specification._
import scala.xml._

class wikiFormatterSpec extends org.spex.Specification {
  "A wiki formatter" should {
    def formatString(s: String): String = new WikiFormatter(){}.format(s)
    def formatElem(e: Elem): Node = new TextileFormatter(){}.format(e)

    "return a string as it is if isn't some html text" in {
      formatString("a description") must_== "a description"
    }
    "format the description of example as some xml text" in {
      val example = new Example("").in { 1 must_== 1 }
      val formatter = new WikiFormatter(){}
      example.exampleDescription = formatter.makeExampleDescription(<ex>a description</ex>)
      formatter.formatDesc(example) must_== <t>a description</t>
    }
    "format single quotes as single quotes inside brackets when using html escape convention ==" in {
      formatElem(<t>==['a description']==</t>) must \\(<p>['a description']</p>)
    }
    "format single quotes as single quotes" in {
      formatString("don't") must include("don't")
    }
  }
  "A wiki formatter setStatus function" should {
    val exampleDesc = "a description"
    val example = "a description" in { 1 must_== 1 }
    val descWithStatus = new WikiFormatter(){}.setStatus(exampleDesc, List(example))

    "set the example status as an xml attribute" in {
      descWithStatus must include("<ex class=\"success\"")
    }
    "set a mouseover function to open a tooltip for the example as an xml attribute" in {
      descWithStatus must include("onmouseover=\"showExampleDesc")
    }
    "set a mouseout function to close the tooltip for the example" in {
      descWithStatus must include("onmouseout=\"hideToolTip();\"")
      descWithStatus must include("a description")
      descWithStatus must beMatching("\\<ex.*\\>.*\\<\\/ex\\>")
    }
    "leave the example description" in {
      descWithStatus must include("a description")
    }
  }
  "A Textile formatter" should { 
    val exampleDesc = "a description"
    val example = new Example("a description")
    example.executeThis
    val descWithStatus = new TextileFormatter(){}.setStatus(exampleDesc, List(example))
    "enclose the description with ex tags protected by wiki markup" in {
      descWithStatus must beMatching("==\\<ex.*\\>a description\\<\\/ex\\>==")
    }
  }
}
