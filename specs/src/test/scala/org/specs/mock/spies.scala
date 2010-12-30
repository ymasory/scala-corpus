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

object spies extends spies
class spies extends HtmlSpecification("Spies") with MockitoSpecification { <wiki>
{"""import org.specs.Specification
  import org.specs.mock.Mockito
  import org.mockito.Mock
  import java.util.List
  import java.util.LinkedList""" prelude it shh }

h3. Spies

You can create spies of real objects. When you use the spy then the real methods are called (unless a method was stubbed).
Real spies should be used carefully and occasionally, for example when dealing with legacy code.

Spying on real objects is often associated with "partial mocking" concept. However, Mockito spies are not partial mocks. 
Mockito spy is meant to help testing other classes, not the spy itself. Therefore spy will not help if you intend to verify if method calls other method on the same object. In this case I suggest being OO/SRPy (for example you might extract new class/interface...)

A spy is created with the @spy@ method: {"""

  class s extends Specification with Mockito {
    val spiedList = spy(new LinkedList[String])
  } 
""" prelude it }

<ex>You can stub out some methods on a spy</ex>:

{ "new s { spiedList.size returns 100 }.spiedList.size" snip it }
{ >("100") }

<ex>Then you can call real methods</ex>: {"""
                                          
   val spec = new s                                       
   spec.spiedList.add("one")
   spec.spiedList.add("two") 
   spec.spiedList""" snip it }

{ >("one, two") }
 
<ex>And you can verify calls</ex>: {"""
                                          
   new s {
     spiedList.add("one")
     there was one(spiedList).add("one") 
   }.isOk""" snip it }

{ >("true") }

h.4 Important gotcha on spying real objects!

<ex>Sometimes it's impossible to use @returns@ for stubbing spies</ex>. For example: {"""
                                          
   new s {
     // Impossible: real method is called so spy.get(0) throws IndexOutOfBoundsException (the list is yet empty)
     spiedList.get(0) returns "one"
   }.failures""" snip it }

{ >("IndexOutOfBoundsException") }
   
<ex>You have to use @doReturn@ for stubbing</ex> {"""
   new s {
     doReturn("one").when(spiedList).get(0)
   }.spiedList.get(0)""" snip it }
{ >("one") }

</wiki> isSus
}
