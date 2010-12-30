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
import org.specs.matcher._
import java.util.regex.Pattern
import org.specs.specification._
/**
 * This trait allows to define mocked method and to specify how they can be called
 * It is used in 3 steps:<ul> 
 * <li>first create a mock object which will override the methods that you want to mock
 *  <pre>val mock = new MyClass {
 *    override def method { record }
 *  }</pre>   
 * <li>implement the mocked methods with the <code>Mocker.record</code> method
 * <li>declare expectations in your specification: <code>expect { mock.method }</code>
 * <li>every expectation will be automatically checked at the end of the example (see the implementation of the
 *  <code>ExampleLifeCycle</code> trait)</ul>
 */
trait Mocker extends ProtocolTypes with LifeCycle with MockMatchers with ExpectationsListener {
  /** protocol storing mocks expectations */
  val protocol = new Protocol

  /** 
   * this variable is used to distinguish uses of the <code>record</code> method. Either during
   * expectation definitions or during actual calls
   */
  private var expectingMode = 0

  /** 
   * expects some methods to be called on mocks. Any call to the <code>record</code> method
   * during the evaluation of v will create a new expectation<br>
   * Usage: <code>expect(twoOf, exclusively) { mock.method }</code>
   */
  def expect(t: ProtocolType, e: Exclusivity)(v: => Any): Protocol = {
    if (expectingMode == 0) protocol.clear
    expectingMode += 1
    if (e == exclusively) 
      protocol.exclusive = true
    protocol.expect(t)(v).isExpectation
    expectingMode -= 1 
    protocol
  }

  /** 
   * default expect method: inAnyOrder, nonExclusively
   */
  def expect(v: => Any): Protocol = expect(inAnyOrder, nonExclusively)(v)

  /** 
   * next default expect method: any protocol type "t", nonExclusively
   */
  def expect(t: ProtocolType)(v: => Any): Protocol = expect(t, nonExclusively)(v)
  
  /** 
   * records a method call.<br> If the expecting mode is > 0, that is, if we are inside an expect { } block
   * then add an expectation with the mocked method name. Otherwise, this is a regular call which is recorded as
   * a received call
   */
  def record: Unit = {
    if (expectingMode > 0) 
      protocol.expectCall(methodName)
    else
      protocol.receiveCall(methodName)
  }

  /** 
   * records a method call and return a specific value.<br>
   * Usage: <code>val mock = new MyClass { override def method: Int = recordAndReturn(1) }</code> 
   * @return 1 on every call to the method <code>method</code>
   */
  def recordAndReturn[T](v: T): T = {
    record
    v
  }
  
  /** 
   * convenience method to add an expectation to check method parameters during calls<br>
   * Usage: <pre>def createMock(f: Movie => Unit) = new MovieRater { 
   *  override def register(m: Movie) =  record(f(m)) 
   * }</pre>
   * then <code> val mock = createMock((m: Movie) => {m.name must notBe(null)}) </code>
   */
  def record[T](v: T): T = {
    record
    v
  }

  /**
   * gets the name of the recorded method by throwing an exception and parsing the stacktrace
   */
  private def methodName = {
    val message = (new Exception()).getStackTrace.toList.dropWhile(!_.toString.contains("record")).dropWhile(_.toString.contains("record"))(0).toString
    val matcherExp = Pattern.compile(".*\\.(.*\\(.*)").matcher(message)
    matcherExp.find
    matcherExp.group(1)
  }

  /**
   * clears the protocol before each example to start with new expectations
   */
  override def beforeExample(ex: Examples) = {
    super.beforeExample(ex)
    protocol.clear
  } 
  /**
   * clears the protocol after each example to start with new expectations
   */
  override def afterExample(ex: Examples) = {
    protocol.clear
    super.afterExample(ex)
  }

  /**
   * checks expectations if some have been made during the test 
   */
  override def afterExpectations(ex: Examples) = {
    if (protocol.isSpecified) {
      ex.addExpectation
      (new Expectation(protocol)) must beMet
    } 
    super.afterExpectations(ex)
  }
  
  /**
   * syntactic sugar allowing to write <pre>
   * expect {
	 * twoOf {  // instead of expect(twoOf) {
   *     mock.call
	 * } 
   * }<pre>
   */
  implicit def protocolTypeToProtocolDef(t: ProtocolType)(v: => Any) = {
    expect(t, nonExclusively)(v)
  }
}

