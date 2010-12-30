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
import org.specs.literate._
import org.specs.execute._

class mockitoSpec extends HtmlSpecificationWithJUnit("Mockito Specification") with MockitoSpecification {

  <wiki>
Mockito is a Java library for mocking.

The following samples are taken from the main documentation which can be found "here":http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/Mockito.html

h3. Let's verify some behaviour

First of all, we need to import some classes and traits for our examples: {"""

  import org.specs.Specification
  import org.specs.mock.Mockito
  import org.mockito.Mock
  import java.util.List
  import java.util.LinkedList""" prelude it }

A mock is created with the @mock@ method: {"""

  object s extends Specification with Mockito {

    // mock creation
    val m = mock[List[String]]

    // using mock object
    m.add("one")
    m.clear()""" snip it }

  // <ex>It is possible to check that some methods have been called on the mock with the @called@ matcher</ex>:

{"""    // verification
    there was one(m).add("one")
    there was one(m).clear()
  } """ add it }{ executeIsNot("error") }

h4. Failures

If one method has not been called on a mock, <ex>the expression @there was one(m).method@ must throw a @FailureException@</ex>: {"""

  object s2 extends Specification with Mockito {
    val m = mock[List[String]]
    there was one(m).clear()
  }
  s2.failures
  """ snip it }
  { >("Wanted but not invoked") }

h4. Argument matchers

{ linkTo(argumentMatchers) } allow flexible verification or stubbing.

h3. How about some stubbing?

<ex>You can mock concrete classes, not only interfaces</ex> {"""

  object s3 extends Specification with Mockito {
    val m = mock[LinkedList[String]]

    // stubbing
    m.get(0) returns "first"
    m.clear() throws new RuntimeException
  }
""" prelude it }{ executeIsNot("error") }

<ex>Calling a stubbed method with @returns@ returns the expected value</ex>. For example, the following prints "first":

{ "s3.m.get(0)" snip it }
{ >("first") }

<ex>Calling a stubbed method with @throws@ throws the expected exception</ex>. For example, the following throws a RuntimeException:

{ "s3.m.clear()" snip it }
{ >("RuntimeException") }

<ex>Calling a non-stubbed method should return a default value</ex>. For example, the following returns @null@ because @get(999)@ was not stubbed:
  
{ "s3.m.get(999)" snip it }
{ >("null") }

h3. Verifying the number of invocations

The number of invocations (atLeast, atMost) can also be checked: { linkTo(numberOfInvocations) }

h3. Verifying that invocations occur in order

When calls have to happen in a given order of invocations, this can be also checked { linkTo(inOrder) }

h3. Callbacks

In some rare case, you want the stubbed return values to be a function of the input method parameters: { linkTo(callbacks) }

h3. Annotations

<ex>It is possible to use annotations to declare mocks</ex> {"""

  object s5 extends Specification with Mockito {
    // do we gain anything using Scala, compared to val mockedList = mock[List[String]]?
    @Mock val m: List[String] = null  
    "this needs to be inside an example because otherwise a NPE is thrown" in {
      m.clear()
      there was one(m).clear()
    }
  }
""" snip it }

{ "s5.isOk" add it }
{ >("true") } 
{ "s5.issues" add_> }

h3. Stubbing consecutive calls (iterator-style stubbing)

Sometimes we need to stub with different return value/exception for the same method call. Typical use case could be mocking iterators. Original version of Mockito did not have this feature to promote simple mocking. For example, instead of iterators one could use Iterable or simply collections. Those offer natural ways of stubbing (e.g. using real collections). 
In rare scenarios stubbing consecutive calls could be useful, though: {"""

  object s6 extends Specification with Mockito {
    val m = mock[List[String]]
    m.get(0) returns "hello" thenReturns "world"
  } """ snip it }

<ex>The first call returns the first value</ex>:

{ "s6.m.get(0)" add it }
{ >("hello") }

<ex>The second call returns the second value</ex>:

{ "s6.m.get(0)" add it }
{ >("world") }

When several values need to be stubbed this version of returns would also work: {"""

  object s7 extends Specification with Mockito {
    val m = mock[List[String]]
    m.get(0) returns ("hello", "world")
  }
""" snip it }

<ex>The first value is "hello"</ex>:
{ "s7.m.get(0)" add it }
{ >("hello") }

<ex>The second value is "world"</ex>:
{ "s7.m.get(0)" add it }
{ >("world") }

h3. Spies

You can create { linkTo(spies) } of real objects. When you use a spy then the real methods are called (unless a method was stubbed).
   
h3. Return values

Speficic { linkTo(returnValues) } can be returned on unstubbed methods.
  
</wiki> isSus

  include(argumentMatchers)
  include(callbacks)
  include(inOrder)
  include(numberOfInvocations)
  include(spies)
  include(returnValues)
}
trait MockitoSpecification extends Mockito with Expectations with LiterateSnippets with Wiki { this: Specification =>
}
