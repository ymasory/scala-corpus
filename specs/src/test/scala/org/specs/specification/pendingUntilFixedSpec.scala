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
package org.specs.specification
import org.specs._

class pendingUntilFixedSpec extends SpecificationWithJUnit {
   "A specification extending PendingUntilFixed" should {
     "mark failing examples as skipped" in {
       object s extends Specification with PendingUntilFixed { 
         "ex" in {       
           pendingUntilFixed { 1 must_== 2 }
         }
       } 
       s.skipped must have size(1)
     }
     "mark passing examples as failed - with a message to remove the pending block" in {
       object s extends Specification with PendingUntilFixed { 
         "ex" in {       
           pendingUntilFixed { 1 must_== 1 }
         }
       } 
       s.skipped must be empty;
       s.failures must have size(1)
       s.failures(0).getMessage must_== "Fixed now. You should remove the 'pending until fixed' declaration"
     }
   }
   "A specification extending PendingUntilFixed" can {
     "use the pendingUntilFixed method on Examples to set the example body as PENDING" in {
       object s extends Specification with PendingUntilFixed { 
         "ex" in { 
           1 must_== 2 
         } pendingUntilFixed
       } 
       s.skipped must have size(1)
     }
     "use the pendingUntilFixed method on a sus to set the examples PENDING" in {
       object s extends Specification with PendingUntilFixed {
         "sus" should {
           "ex" in { 1 must_== 2 } 
           "ex2" in { 1 must_== 2 }
         } pendingUntilFixed
       } 
       s.skipped must have size(2)
     }
   }
}
