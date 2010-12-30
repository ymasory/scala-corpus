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

import _root_.org.easymock.classextension.{ EasyMock => EasyClassMocker }
import _root_.org.easymock.{ IExpectationSetters, IAnswer, EasyMock => EasyMocker }
import org.easymock.internal.MocksControl._
import org.specs.specification.{ ExpectationsListener, LifeCycle, Examples } 
import org.specs.execute.FailureException
import org.specs.util.ExtendedThrowable._
import java.lang.reflect.Proxy
import org.easymock.internal._

/**
 * This trait integrates EasyMock in specs so that mock exceptions are correctly 
 * reported as specs failures and adds some syntactic sugar to allow an easier syntax.
 */
trait EasyMock extends ExpectationsListener with EasyMockLifeCycle {
  /** 
   * create a mock with a name
   * @return the mock object
   */
  def mockAs[T <: Object](name: String)(implicit m: scala.reflect.ClassManifest[T]): T = {
    val m1 = EasyClassMocker.createNiceMock(name, m.erasure).asInstanceOf[T]
    mocks.append(m1)
    m1
  }
  /** 
   * create a nice mock with a name
\   * @return the mock object
   */
  def niceMockAs[T <: Object](name: String)(implicit m: scala.reflect.ClassManifest[T]): T = {
    val m1 = EasyClassMocker.createNiceMock(name, m.erasure).asInstanceOf[T]
    mocks.append(m1)
    m1
  }
  /** 
   * create a strict mock with a name
   * @return the mock object
   */
  def strictMockAs[T <: Object](name: String)(implicit m: scala.reflect.ClassManifest[T]): T = {
    val m1 = EasyClassMocker.createStrictMock(name, m.erasure).asInstanceOf[T]
    mocks.append(m1)
    m1
  }
  /** 
   * create a mock
   * @return the mock object
   */
  def mock[T <: Object](implicit m: scala.reflect.ClassManifest[T]): T = {
    val m1 = EasyClassMocker.createMock(m.erasure).asInstanceOf[T]
    mocks.append(m1)
    m1
  }
  /** 
   * create a nice mock
   * @return the mock object
   */
  def niceMock[T <: Object](implicit m: scala.reflect.ClassManifest[T]): T = {
    val m1 = EasyClassMocker.createNiceMock(m.erasure).asInstanceOf[T]
    mocks.append(m1)
    m1
  }
  /** 
   * create a strict mock
   * @return the mock object
   */
  def strictMock[T <: Object](implicit m: scala.reflect.ClassManifest[T]): T = {
    val m1 = EasyClassMocker.createStrictMock(m.erasure).asInstanceOf[T]
    mocks.append(m1)
    m1
  }
  /** 
   * expect some calls and replay all mocks which have been previously declared
   */
  def expect[T](t: =>T):T = {
    val result = t
    mocks.foreach { m => 
      EasyClassMocker.replay(m)
    }
    result.isExpectation
  }
  /** @return an object supporting the convienience methods on mock objects */
  implicit def theMock[T  <: Object](c: =>T) = new MockedObject(c)
  /** 
   * This class provide mock methods like reset, replay, verify, toNice,....
   * Internally it calls the corresponding methods on EasyMocker
   */
  class MockedObject[T <: Object](c: =>T) {
    def toNice = EasyClassMocker.resetToNice(c)
    def toDefault = EasyClassMocker.resetToDefault(c)
    def toStrict = EasyClassMocker.resetToStrict(c)
    def reset = EasyClassMocker.reset(c)
    def replay = EasyClassMocker.replay(c)
    def verify = EasyClassMocker.verify(c)
    def checkOrder = EasyClassMocker.checkOrder(c, true)
    def dontCheckOrder = EasyClassMocker.checkOrder(c, false)
    def checkIsUsedInOneThread = EasyMocker.checkIsUsedInOneThread(c, true)
    def dontCheckIsUsedInOneThread = EasyMocker.checkIsUsedInOneThread(c, false)
    def makeThreadSafe = EasyClassMocker.makeThreadSafe(c, true)
    def dontMakeThreadSafe = EasyClassMocker.makeThreadSafe(c, false)
  }
  /** @return an object supporting the convienience methods on calls. */
  implicit def theCall[T](c: =>T) = new CalledObject(c)

  /** 
   * This class provide mock methods like returns, answers,....
   * Internally it calls the corresponding methods on EasyMocker (andReturn, andAnswer,...)
   */
  class CalledObject[T](c: =>T) {
    def returns(t: T) = EasyMocker.expect(c).andReturn(t)
    def stubReturns(t: T) = EasyMocker.expect(c).andStubReturn(t)
    def throws[E <: Throwable](e: E) = EasyMocker.expect(c).andThrow(e)
    def answers(function: () => T) = EasyMocker.expect(c).andAnswer(new MockAnswer2(function))
    def answers(function: Any => T) = EasyMocker.expect(c).andAnswer(new MockAnswer(function))
    def delegatesTo[S](other: S) = EasyMocker.expect(c).andDelegateTo(other)
    def stubDelegatesTo[S](other: S) = EasyMocker.expect(c).andStubDelegateTo(other)
    def times(i: Int) = {
      val result = c
      EasyMocker.expectLastCall.times(i)
      result
    }
    def atLeastOnce = {
      val result = c
      EasyMocker.expectLastCall.atLeastOnce
      result
    }
    def anyTimes = {
      val result = c
      EasyMocker.expectLastCall.anyTimes
      result
    }
    def times(i: Int, i2: Int) = {
      val result = c
      EasyMocker.expectLastCall.times(i, i2)
      result
    }
    /** 
     * This class is an implementation of the IAnswer interface allowing to pass functions as an answer.
     * 
     * It does a bit of work for the client:
     * 
     * // if the method has one parameter and the function also, the parameter is passed
     * mock.get(0) answers ( i => i.toString )
     * 
     * In any other case, the array of the method parameters will be passed to the function.
     * 
     */
     class MockAnswer[T](function: Any => T) extends IAnswer[T] {
       def answer: T = {
         val args = EasyMocker.getCurrentArguments
         if (args.size == 0) {
           function match {
             case f: Function0[_] => f()
           }
         } else if (args.size == 1) {
           function match {
             case f: Function1[_, _] => f(args(0))
           }
         } else {
           function match {
             case f: Function1[_, _] => f(args)
           }
         }
       } 
     }
     /**
      * specific implementation of the Answer interface for a function taking no parameters
      */
     class MockAnswer2[T](function: () => T) extends IAnswer[T] {
       def answer: T = function()
     }
  }
  /** @return an object supporting the chained expectations. */
  implicit def theExpectations[T](c: =>IExpectationSetters[T]) = new Expectations(c)
  /** 
   * This class provides a way to chain expectations on mocked calls with the same syntax as
   * the one introduced by specs
   */
  class Expectations[T](c: =>IExpectationSetters[T]) {
    def andReturns(t: T) = c.andReturn(t)
    def andStubReturns(t: T) = c.andStubReturn(t)
    def andThrows[E <: Throwable](e: E) = c.andThrow(e)
    def andDelegatesTo[S](other: S) = c.andDelegateTo(other)
    def andStubDelegatesTo[S](other: S) = c.andStubDelegateTo(other)
  }
  /**
   * replay one or several mocks 
   */
  def replay[T <: Object](m: T*) = m.foreach(EasyClassMocker.replay(_))
  /**
   * verify one or several mocks. Each verify is counted as an expectation 
   */
  def verify[T <: Object](m: T*) = {
    try {
      m.foreach(EasyClassMocker.verify(_).isExpectation)
    } catch {
      case e: java.lang.AssertionError => {
        val f = new FailureException(e.getMessage)
        f.setStackTrace(e.getStackTrace)
        f.hideCallerAndThrow("(easymock\\.|mock\\.EasyMock)")
      }
    }
  }
}
/**
 * This trait transforms EasyMock errors into specs failures
 */
trait EasyMockLifeCycle extends LifeCycle {
  /** list of mocks to replay in an expect block */
  private[mock] val mocks = new scala.collection.mutable.ListBuffer[Object]

  override def executeExpectations(ex: Examples, t: =>Any) = {
    try { 
      super.executeExpectations(ex, t)
    }
    catch {
      case e if (e.getStackTrace.exists(_.toString.contains("easymock"))) => {
        val f = new FailureException(e.getMessage)
        f.setStackTrace(e.getStackTrace)
        f.hideCallerAndThrow("(easymock\\.|mock\\.EasyMock)")
      }
    }
  }
}

