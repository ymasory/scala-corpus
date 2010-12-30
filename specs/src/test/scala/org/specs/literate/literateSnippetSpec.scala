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
import org.specs.util._

class literateSnippetSpec extends HtmlSpecificationWithJUnit("Literate snippet") with Examples with LiterateSnippets { <wiki>

h3. Declaration

In Literate specifications it often desirable to show the behaviour of some code:{ """
  val a = 1
  val b = 1
  a + b
""" snip (it) }

<ex>The code above should be executable and return the value "2"</ex>

> { executeAndExpect("2") }

h3. Execution

<ex>Snipping a new piece of code should remove the previous one</ex>: { """
  var a = 2
  var b = 3
  a + b
""" snip (it) }

> { executeAndExpect("5") }

But <ex>it is possible to add a piece of code to a previous snippet with @addTo@</ex>:
{ """
  b = 6
  a + b
""" addTo (it) }

> { executeAndExpect("8") }


h3. Prelude

<ex>Imports can be added to a snippet variable with @prelude@ and be persistent even if the code is snipped again</ex>:
{ "import java.util.ArrayList" prelude (it) }
{ """val l = new ArrayList[Int]()
     l.add(3)
     l.get(0)""" snip (it) }

> { executeAndExpect("3") }


</wiki> isSus
}
trait Examples extends SnipIt with Expectations {
  def executeAndExpect(expected: String) = {
    val result = execute(it)
    result aka it.get.code must include(expected)
    result
  }
}
