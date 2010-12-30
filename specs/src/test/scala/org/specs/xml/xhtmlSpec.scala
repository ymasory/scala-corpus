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
package org.specs.xml
import Xhtml._

class xhtmlSpec extends org.spex.Specification {
  "The Xhtml object" should {
    "have a spanLastTd function setting a colspan on each last td of a row, except the last one" in {
      val updated = Xhtml.spanLastTd(
        <table class="dataTable">
          <tr><th>person</th></tr>
          <tr><td>First Name</td><td>Eric</td><td>Last Name</td><td>Torreborre</td></tr>
        </table>)
      updated must \\(<th>person</th>, Map("colspan"->"4"))
      updated must not(\\(<td>Torreborre</td>, Map("colspan"->"4")))
    }
    "have a spanLastTd function setting a colspan on each last td of a row, except the last one - 2" in {
      val updated = Xhtml.spanLastTd(
        <table class="dataTable">
          <tr><th>person</th></tr>
          <tr><td>First Name</td><td><b>Eric</b></td></tr>
          <tr><td>First Name</td><td>Eric</td><td>Last Name</td><td>Torreborre</td></tr>
        </table>)
      updated must \\(<th>person</th>, Map("colspan"->"4"))
      updated must not(\\(<td>Torreborre</td>, Map("colspan"->"4")))
    }
  }
  "The Xhtml object" should {
    "have a maxColSize function computing the maximum size of a row even with <b/> tags nested inside a <td/> tag" in {
      maxColSize(
        <table>
          <tr><td>4/14/2009</td><td>-10</td><td><b>A</b>B</td></tr>
        </table>) must ==(3)
    }
    "have a maxColSize function computing the max size even with empty cells" in {
      maxColSize(<table>
        <tr> 
          <th>Messages</th> 
        </tr> 
        <tr> 
          <td class="info"></td> 
          <td class="info">RECEIPT</td> 
        </tr> 
      </table>) must_== 2
    }
  }
}
