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
import org.specs.execute._

/**
 * This trait allows examples to be marked as PENDING if their body is failing.
 * 
 * The usage is: <code>
 *  object s extends Specification with PendingUntilFixed { 
 *    "ex" in {       
 *       pendingUntilFixed { 1 must_== 2 }
 *     }
 *  } 
 * </code>
 * 
 * It is also possible to mark the example, or a whole sus as pendingUntilFixed with: <code>
 *  object s extends Specification with PendingUntilFixed { 
 *    "ex" in {       
 *       1 must_== 2
 *     } pendingUntilFixed
 *  } 
 *  object s2 extends Specification with PendingUntilFixed {
 *    // all examples will be pendingUntilFixed
 *    "sus" should {
 *      "ex1" in { 1 must_== 2 } 
 *      "ex2" in { 1 must_== 2 } 
 *    } pendingUntilFixed
 *  } 
 * </code>
 * If the example body is passing, then the example will fail with a message
 * warning the user that the pendingUntilFixed block can be removed
 */
trait PendingUntilFixed { outer =>
  /** implicit definition to add pendingUntilFixed ability to an example*/
  implicit def toPendingExample(e: Examples) = new PendingExample(e)
  class PendingExample(e: Examples) {
    def pendingUntilFixed = {
      def makePending(a: =>Any) = outer.pendingUntilFixed(a)
      e.aroundExpectations = Some(makePending(_)) 
      e
    }
  }
  def pendingUntilFixed(f: =>Any) { 
    val isPassing = 
      try { 
        f 
        true
      } 
      catch { case _ => false } 
      if (isPassing) 
        throw new FailureException("Fixed now. You should remove the 'pending until fixed' declaration") 
      else 
        throw new SkippedException("Pending until fixed") 
  } 
}
