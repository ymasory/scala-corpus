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

object returnValues extends returnValues
class returnValues extends HtmlSpecification("Return values") with MockitoSpecification {
<wiki>

h3. Return values

Optional ReturnValues can be used with mock[Class](ReturnValues). "ReturnValues":http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/ReturnValues.html defines the return values of unstubbed invocations.

This implementation can be helpful when working with legacy code. Unstubbed methods often return null. If your code uses the object returned by an unstubbed call you get a NullPointerException. This implementation of ReturnValues makes unstubbed methods return SmartNull instead of null. SmartNull gives nicer exception message than NPE because it points out the line where unstubbed method was called. 
  You just click on the stack trace.

SmartNullReturnValues can be set on mocks with the smartMock method.
It first tries to return ordinary return values (see "MoreEmptyReturnValues":http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/internal/returnvalues/MoreEmptyReturnValues.html) then it tries to return SmartNull. If the return type is final then plain null is returned.

{"""  
  import org.specs._
  import org.specs.mock.Mockito""" prelude it shh }
 
For Example: {"""

  class s1 extends Specification with Mockito {
    val got = mock[org.specs.mock.Hello].get(0)
  } """ snip it }

<ex>The returned value should yield a NullPointerException</ex>: 
  
{ "new s1().got.toString" add it } 
{ >("NullPointerException")}

If @smartMock@ is used: {"""

  class s2 extends Specification with Mockito {
    val got = smartMock[org.specs.mock.Hello].get(0)
  } """ snip it }

<ex>Accessing the returned value will yield an empty string instead of @null@</ex>: 
  
{ "new s2().got" add it } 
{ >("")}


  </wiki> isSus
}
trait Hello {
  def get(i:Int): String
}

