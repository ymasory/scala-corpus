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

/**
 * Specifies which kind of received calls can be expected
 */
abstract class SpecifiedCall {
  
  /** the <code>repetition</code> parameter specifies how much this specified call is expected to happen*/
  var repetition: CallConstraint = atLeastN(0)

  /**
   * tries to mark received messages as consume when they can be expected by expected calls<br>
   * If an expected calls has received everything that was expected, it is marked as "passes"
   */
  def consume(received: List[ReceivedCall]): (List[SpecifiedCall], List[ReceivedCall])

  /**
   * removes any memory of what has been previously matched
   */
  def clear: Unit

  /**
   * @return <code>true</code> if every expected method has been properly called
   */
  def passes: Boolean = false
}

/**
 * Specifies that a method is expected to be called. By construction, the method name will include the line number
 * in the original source file. The precondition is that 2 different expected methods should have 2 different
 * names (so even in cases where a method is overloaded)
 */
case class ExpectedCall(val method: String) extends SpecifiedCall {
  
  /** number of times that the method <code>method</code> has been called */
  var callsNumber = 0

  /** clears the number of times the method has been called */
  def clear = callsNumber = 0

  /** @return true if the method has been called a sufficient number of times, defined by <code>repetition</code> */
  override def passes : Boolean = repetition.verifies(callsNumber)

  /** 
   * tries to consume every received call, until:<ul>
   * <li>a matching received unconsumed call (with the same method name) is found
   * <li>the repetition criteria indicates that we can stop</ul>
   * @return a pair with the list of expected calls, i.e. this one and the list of received calls 
   */
  def consume(received: List[ReceivedCall]) = {
    var found = false
    received takeWhile(r => {
    // ("trying " + method + " with " + r + " consumed " + r.consumed + " found " + found).pln
      if (r.method == method && !found && !r.consumed){
        callsNumber += 1
        r.consumedBy = Some(this)
        if (repetition.stop(callsNumber))
          found = true
        else
          found = false
        !found
      }
      else
        true
    })
    (List(this), received)
  }
   
  override def toString = method
}

/**
 * Specifies that a method has been received.<br> 
 * By construction, the method name will include the line number in the original source file. 
 */
case class ReceivedCall(val method: String) {
  
  /** stores the expected calls having consumed this received call */
  var consumedBy: Option[SpecifiedCall] = None

  /** @return true if the method has been expected by an expected call */
  def consumed = (consumedBy != None)

  override def toString = method
}

/**
 * Specifies that a set of methods or other protocol definitions are expected to be called
 * according to a protocol type
 */
case class ProtocolDef(val protocolType: ProtocolType, var expectedCalls: List[SpecifiedCall]) extends SpecifiedCall with ProtocolTypes {

  /** adds a method name to the list of expected calls */
  def expect(m: String) = expectedCalls = expectedCalls:::List(ExpectedCall(m))

  /** adds an inner protocol definition to this one */
  def expect(p: ProtocolDef) = expectedCalls = expectedCalls:::List(p)

  /** consumes the received calls, according to the expected ones and the protocol type (inAnyOrder, inSequence,...) */
  def consume(received: List[ReceivedCall]) = protocolType.consume(expectedCalls, received)

  /** @return the list of failures computed by the protocol type */
  def failures(rs: List[ReceivedCall], exclusive: Boolean): String = protocolType.failures(expectedCalls, rs, exclusive)

  /** clears the specified calls so that another match can be attempted */
  def clear = expectedCalls foreach {_.clear}

  /** @return true if every specified call passes */
  override def passes : Boolean = expectedCalls.forall(_.passes)

  /** @return a user-friendly description of the expected calls */
  override def toString = protocolType.expectedDefs(expectedCalls)
}

