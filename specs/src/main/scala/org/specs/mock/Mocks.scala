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
import scala.collection.mutable.Stack
import org.specs.collection.ExtendedList._

/**
 * The <code>Protocol</code> class stores the expectations of mocks, alongside with the actual received calls
 * It can be exclusive or not: i.e. return an error in case of unexpected calls or not<br>
 * A <code>protocol</code> has 3 functions:<ul>
 * <li>be defined by nested protocol definitions and expected calls
 * <li>store received calls
 * <li>return failures if there are unmatched calls</ul>
 */
class Protocol extends ProtocolTypes {
  /** actual calls received by mocks */
  var receivedCalls: List[ReceivedCall] = Nil

  /** if true, will check unexpected calls too */
  var exclusive = false

  /** 
   * protocol definitions which are stacked during the protocol creation
   * but which will be nested after creation.<br> Each protocol definition corresponds to 
   * a block of an "expect" declaration:<pre>
   * <code>expect(twoOf) {
   *    expect(inSequence) {
   *      mock.callMethod1
   *      mock.callMethod2
   *    }
   * }</code></pre>
   */
  private var allDefinitions: Stack[ProtocolDef] = new Stack
  
  /** 
   * definition of the protocol 
   */
  def definition = allDefinitions.top

  /** 
   * takes a value <code>v</code>which will declare some mocks expectations<br>
   * By default the protocol type is <code>inAnyOrder</code>
   */
  def expect(v: => Any): ProtocolDef = expect(inAnyOrder)(v)

  /** 
   * takes a value <code>v</code>which will declare some mocks expectations<br>
   * The protocol type is specified by the parameter <code>t</code>
   */
  def expect(t: ProtocolType)(v: => Any): ProtocolDef = {
    // open a new protocol definition of type t with no expected calls
    allDefinitions.push(new ProtocolDef(t, Nil))
    // v is supposed to contain mock expectations
    v
    
    // if there are more than one protocol definition
    // the last protocol on the stack is "poped" and becomes a specified call for the previous protocol definition
    if (allDefinitions.size > 1) {
      val p = allDefinitions.pop
      allDefinitions.top.expect(p)
      p
    }
    else
      allDefinitions.top // else return the outermost protocol
  }

   /** 
    * adds an expected method name to the current protocol definition 
    */
   def expectCall(methodName: String) = allDefinitions.top.expect(methodName)

   /** 
    * adds a received call to the list of received calls 
    */
   def receiveCall(methodName: String) = receivedCalls = receivedCalls:::List(new ReceivedCall(methodName))

  /** 
   * @return an error message if the protocol definition 
   * has some expected calls which don't match the received calls
   */
  def failures: String = {
    val f = definition.failures(receivedCalls, exclusive)
    // if there are no failures that are the result of unmatched calls
    // and if the protocol is defined as exclusive, return a message with unexpected calls if there are some
    if (exclusive && f.isEmpty)
      receivedCalls.filter(!_.consumed).map(_.toString + " should not have been called").mkString("\n")
    else
      f
  }
  
  /** 
   * A <code>Protocol</code> is specified if there it contains one protocol definition 
   */
  def isSpecified = !allDefinitions.isEmpty

  /** 
   * Removes any previous protocol definition and received calls 
   */
  def clear = {
    allDefinitions = new Stack
    receivedCalls = Nil
  }
}

