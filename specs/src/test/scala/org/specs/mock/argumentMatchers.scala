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

object argumentMatchers extends argumentMatchers
class argumentMatchers extends HtmlSpecification("Argument Matchers") with MockitoSpecification {
<wiki>

h3. Argument matchers

Argument matchers allow flexible verification or stubbing. Click "here":http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/Matchers.html for more details.

Let's import some definitions first: {"""  

  import org.specs._
  import org.specs.mock.Mockito
  import java.util.List
  import org.mockito.Matchers.{ argThat, anyInt, eq => isEq }
  import org.hamcrest.core.{ IsNull }
""" prelude it }

And create a specification with mocks: {"""

  object args1 extends Specification with Mockito {
    val m = mock[List[String]]
    
    // stubbing using built-in anyInt() argument matcher
    m.get(anyInt()) returns "element"

    // stubbing using hamcrest (let's say IsNull returns your own hamcrest matcher):
    m.contains(argThat(new IsNull[String])) returns true
  }
""" prelude it }

Then, <ex>calling the mocked list with any argument must return "element"</ex>: 
  
{ "args1.m.get(999)" snip it } 
{ >("element")}

and <ex>calling the mocked list @contains@ method with a valid argument must return "true" if the passed argument is null</ex>:
  
{ "args1.m.contains(null)" snip it } 
{ >("true")}

<ex>It is also possible to verify that a mock was called with an argument matcher</ex>: {"""

  object args2 extends Specification with Mockito {
    args1.m.get(999)
    there was one(args1.m).get(isEq(999))
  }
  args2.successes""" snip it }
  { >("example 1")}
 
<ex>Instead of Hamcrest matchers, a specs matcher can be used</ex>:{"""
  object args3 extends Specification with Mockito {
    val m = mock[List[String]]
    m.get(==(123)) returns "one"
  }
  args3.m.get(123)
  """ snip it } 
  { >("one")}
  
  </wiki> isSus
}
