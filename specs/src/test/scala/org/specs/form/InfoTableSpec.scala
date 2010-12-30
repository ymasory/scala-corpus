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
package org.specs.form
import org.specs._
import org.specs.Sugar._

class InfoTableSpec extends SpecificationWithJUnit {
  "An info table can be created with a title" in {
    new InfoTable("title").title must_== "title"
  }
  "An info table can be created with some column names for the header" in {
    val table = new InfoTable("title") {
      header("column1", "column2")
    }
    table.toXhtml must \\(<th>column1</th>)
  }
  "An info table can have a line even if the header is missing" in {
    val table = new InfoTable("title") {
      line("value11", "value12")
    }
    table.toXhtml must \\(<td>value11</td>)
  }
  "An info table can have lines with string values" in {
    val table = new InfoTable("title") {
      header("column1", "column2")
      line("value11", "value12")
      line("value21", "value22")
    }
    table.toXhtml must \\(<td>value11</td>) and
                       \\(<td>value22</td>)
  }
  "An info table has lines with values as info values" in {
    val table = new InfoTable("title") {
      line("value11", "value12")
    }
    table.toXhtml must \\(<td class="info">value11</td>)
  }
  "An info table can have lines with values as success values" in {
    val table = new InfoTable("title") {
      line("value11".successValue, "value12")
    }
    table.toXhtml must \\(<td>value11</td>, "class"->"success")
  }
}
