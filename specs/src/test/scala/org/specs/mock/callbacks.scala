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
package org.specs.mock
import org.specs._

object callbacks extends callbacks
class callbacks extends HtmlSpecification("Stubbing with callbacks") with MockitoSpecification {
<wiki>

h3. Stubbing with callbacks

Allows stubbing with generic "Answer":http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/stubbing/Answer.html interface.
Yet another controversial feature which was not included in Mockito originally. 
We recommend using simple stubbing with @returns@ or @throws@ only. Those two should be just enough to test/test-drive any clean and simple code.
{"""  
  import org.specs._
  import org.specs.mock.Mockito
  import org.mockito.Matchers.{ anyInt }
  import java.util.List """ prelude it shh }
 
Here is a specification where the stubbed return values depend on the method parameters: {"""

  object s extends Specification with Mockito {
    val m = mock[List[String]]
    
    // stubbing using built-in anyInt() argument matcher
    m.get(anyInt()) answers { i => "The parameter is " + i.toString } 
  }
""" snip it }

<ex>The function passed to @returns@ will be called with each parameter passed to the stubbed method</ex>: 
  
{ "s.m.get(0)" add it } 
{ >("The parameter is 0")}

<ex>The second call returns a different value</ex>:
  
{ "s.m.get(1)" add it } 
{ >("The parameter is 1")}

  </wiki> isSus
}
