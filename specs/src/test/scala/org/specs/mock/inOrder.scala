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

object inOrder extends inOrder
class inOrder extends HtmlSpecification("In order calls") with MockitoSpecification {
<wiki>

h3. Checking that calls occur in the right order

{"""  
  import org.specs._
  import org.specs.mock.Mockito
  import org.mockito.Matchers.{ anyInt }
  import java.util.List """ prelude it shh }
 
With the following mocks: {"""

    class s extends Specification with Mockito {
      import java.util.List
      val m1 = mock[List[String]]
      val m2 = mock[List[String]]
    
      m1.get(0)
      m2.get(0)
   }
""" prelude it }

<ex>We can check that some calls happen in a given order</ex>: 
  
{ """new s {
      there was atLeastOne(m1).get(0) then
                one(m2).get(0) orderedBy (m1, m2)
     }.isOk""" snip it } 
{ >("true")}

<ex>If they don't, there should be a failure message</ex>: 
  
{ """new s {
      there was one(m2).get(0) then
                atLeastOne(m1).get(0) orderedBy (m1, m2)
     }.failures.head.getMessage""" snip it } 
{ >("The mock was not called as expected:")}

  </wiki> isSus
}
