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
import org.specs.Sugar._
import org.specs.collection.ExtendedList._

case object inSequence extends inSequence(exactlyN(1))

/**
 * The <code>inSequence</code> protocol type will try to consume expected calls
 * in sequence. It will not consume unexpected calls.<br>
 * It accepts a <code>repetition</code> parameter specifying how many expected calls are supposed to happen:<ul>
 * <li>exactlyN(2): exactly 2 times
 * <li>atLeast(2): at least 2 times
 * <li>atMost(2): at most 2 times</ul>
 */
class inSequence(repetition: CallConstraint) extends ProtocolType(repetition) {

 /**
  * @return a String specifying the constraints of this protocol: e.g. "in sequence 2 of:".
  * If it is exactly one, returns "in sequence"
  */
  def constraints = {
     repetition match{
       case exactlyN(n) if (n == 1) =>  "in sequence"
       case _ => "in sequence " + repetition.expectation
     }
   }
  
  /**
   * Tries to match expected calls with received calls in sequence
   * until the <code>repetition</code> parameter is satisfied.<br>
   * Before doing so, it sets the repetition number on expected calls, so that they
   * know when to stop matching received calls (especially for atLeast and atMost constraints).<br>
   * If consumed received calls are not in the same order as their respective expected calls
   * it resets them so that the received calls are not consumed and the expected calls are not passed
   * @return the list of expected calls and the list of received calls
   */
  def consume(exp: List[SpecifiedCall], rec: List[ReceivedCall]) = {
    exp.foreach(_.repetition = repetition)
    var n = 0
    do {    
      exp foreach (_.consume(rec))
      if (hasConsumedCallsNotInSequenceWithPassedCalls(exp, rec)) {
        rec foreach (_.consumedBy = None)
        exp foreach {_.clear}
      }
      n = n + 1
    } while (!repetition.verifies(n))
    (exp, rec)
  }
   
  private def hasConsumedCallsNotInSequenceWithPassedCalls(exp: List[SpecifiedCall], rec: List[ReceivedCall]) = {
    rec.exists(r1 =>
      r1.consumed && rec.exists(r2 =>
        r2.consumed && 
           (exp.indexOf(r1.consumedBy.get) > exp.indexOf(r2.consumedBy.get)) &&
           (rec.indexOf(r1) < rec.indexOf(r2))  
      )
    )
 }
}
