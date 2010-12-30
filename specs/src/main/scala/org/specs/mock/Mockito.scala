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
import org.specs.util.Control._
import org.specs.specification._
import org.specs.NumberOfTimes
import org.mockito.InOrder
import org.mockito.stubbing.Answer
import org.mockito.internal.stubbing.StubberImpl
import org.mockito.invocation.InvocationOnMock
import org.mockito.internal.InOrderImpl 
import org.mockito.verification.{ VerificationMode }
import org.mockito.internal.stubbing._
import org.mockito.stubbing.{ OngoingStubbing, Stubber }
import org.specs.matcher._
import org.specs.matcher.MatcherUtils._
import org.specs._

/**
 * The Mockito trait can be mixed with Specifications to provide mocking capabilities using the Mockito library.
 * 
 * It provides some syntactic sugar on top of the Mockito methods and is integrated with specs expectations:<code>
 * 
 * class specification extends Specification with Mockito {
 *   
 *   val m = mock[java.util.List[String]] // a concrete class would be mocked with: mock(new java.util.LinkedList[String])
 *   
 *   // stub a method call with a return value
 *   m.get(0) returns "one"
 * 
 *   // call the method
 *   m.get(0)
 * 
 *   // verify that the call happened, this is an expectation which will throw a FailureException if that is not the case
 *   there was one(m).get(0)
 *   there was no(m).get(1) // we can also check that other calls did not occur
 * }
 * </code>
 * 
 * See the method descriptions for more usage examples.  
 */
trait Mockito extends MockitoLifeCycle with CalledMatchers with MockitoStubs with MockitoMatchers with MockitoFunctions 

/**
 * This trait provides methods to create mocks and spies.
 */
trait MocksCreation extends TheMockitoMocker {
  /**
   * create a mock object: val m = mock[java.util.List[Stringg]]
   */
  def mock[T](implicit m: scala.reflect.ClassManifest[T]): T = mocker.mock(m)
  /**
   * create a mock object with a name: val m = mockAs[java.util.List[String]]("name")
   */
  def mockAs[T](name: String)(implicit m: scala.reflect.ClassManifest[T]): T = mocker.mock(name)(m)
  /**
   * implicit allowing the following syntax for a named mock: val m = mock[java.util.List[String]],as("name")
   */
  implicit def mockToAs[T](t: =>T)(implicit m: scala.reflect.ClassManifest[T]) = new NamedMock(t)(m)
  
  /** support class to create a mock object with a name */
  class NamedMock[T](t: =>T)(implicit m: scala.reflect.ClassManifest[T]) {
    def as(name: String): T = mockAs[T](name)
  }

  /**
   * create a mock object with smart return values: val m = smartMock[java.util.List[Stringg]]
   * 
   * This is the equivalent of Mockito.mock(List.class, SMART_NULLVALUES) but testing shows that it is not working well with Scala.
   */
  def smartMock[T](implicit m: scala.reflect.ClassManifest[T]): T = mocker.smartMock(m)
  /**
   * create a spy on an object. 
   * 
   * A spy is a real object but can still have some of its methods stubbed. However the syntax for stubbing a spy is a bit different than 
   * with a mock:<code>
   * 
   * val s = spy(new LinkedList[String])
   * doReturn("one").when(s).get(0) // instead of s.get(0) returns "one" which would throw an exception
   * 
   * </code>
   */
  def spy[T](m: T): T = mocker.spy(m)
}

/**
 * This trait allows the initialization of mocks when defined with an annotation:
 * @Mock val l: List[String] = null
 * 
 * The beforeExpectations method is overriden instead of the beforeExample method. This doesn't make a big difference but it helps the compiler
 * in the Eclipse plugin not to crash too often.
 */
trait MockitoLifeCycle extends LifeCycle {
  /** variable used to avoid multiple initializations. */
  private var initialized = false

  /** The mocks are reinitialized before each tests. */
  override def beforeExpectations(e: Examples) = {
	super.beforeExpectations(e)
	if (!initialized) {
	  initialized = true
      org.mockito.MockitoAnnotations.initMocks(this)
    }  
  }
}
/**
 * This trait provides methods to declare expectations on mock calls:<code>
 * 
 * there was one(mockedList).get(0)
 * there was no(mockedList).get(0)
 * 
 * there was two(mockedList).get(0)
 * there was three(mockedList).get(0)
 * there was 4.times(mockedList).get(0)
 *
 * there was atLeastOne(mockedList).get(0)
 * there was atLeastTwo(mockedList).get(0)
 * there was atLeastThree(mockedList).get(0)
 * there was atLeast(4)(mockedList).get(0)
 * there was atMostOne(mockedList).get(0)
 * there was atMostTwo(mockedList).get(0)
 * there was atMostThree(mockedList).get(0)
 * there was atMost(4)(mockedList).get(0)
 * 
 * It is also possible to use a different wording:
 * 
 * there were two(mockedList).get(0)
 * got { two(mockedList).get(0) }
 * 
 * </code>
 */
trait CalledMatchers extends ExpectableFactory with NumberOfTimes with TheMockitoMocker {
  /** temporary InOrder object to accumulate mocks to verify in order */
  private var inOrder: Option[InOrderImpl] = None
  /** this matcher evaluates an expression containing mockito calls verification */
  private class CallsMatcher[T] extends Matcher[T] {
    def apply(v: =>T) = {
      var result = (true, "The mock was called as expected", "The mock was not called as expected")
      try { 
        v 
      } catch {
        case e => { 
          result = (false, "The mock was called as expected", "The mock was not called as expected: " + e.getMessage)
        }
      }
      result
    }
  }
  
  /** create an object supporting 'was' and 'were' methods */
  def there = new Calls
  /** 
   * class supporting 'was' and 'were' methods to forward mockito calls to the CallsMatcher matcher 
   */
  class Calls {
    def were[T](calls: =>T) = was(calls)
    def was[T](calls: =>T) = {
      calls must new CallsMatcher[T]
    }
  }
  /**
   * alias for 'there was'
   */
  def got[T](t: =>T) = there was t
  /**
   * implicit definition to be able to declare a number of calls 3.times(m).clear()
   */
  implicit def rangeIntToTimes(r: RangeInt) = new RangeIntToTimes(r)
  /**
   * class providing a apply method to be able to declare a number of calls:
   *   3.times(m).clear() is actually 3.times.apply(m).clear()
   */
  class RangeIntToTimes(r: RangeInt) {
    def apply[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.times(r.n))
  }
  /**
   * verify that a mock has been called appropriately
   * if an inOrder object has been previously created (which means we're verifying the mocks calls order),
   * then the mock is added to the inOrder object and the inOrder object is used for the verification.
   * 
   * Otherwise a normal verification is performed
   */
  private def verify[T <: AnyRef](mock: =>T, v: VerificationMode) = {
    inOrder map { ordered => 
      val mocksList = ordered.getMocksToBeVerifiedInOrder()
      if (!mocksList.contains(mock)) {
        mocksList.add(mock)
        inOrder = Some(new InOrderImpl(mocksList))
      }
    }
    mocker.verify(inOrder, mock, v)
  }
  /** no call made to the mock */
  def no[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.never())
  /** one call only made to the mock */
  def one[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.times(1))
  /** two calls only made to the mock */
  def two[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.times(2))
  /** three calls only made to the mock */
  def three[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.times(3))
  /** at least n calls made to the mock */
  def atLeast[T <: AnyRef](i: Int)(mock: =>T) = verify(mock, org.mockito.Mockito.atLeast(i))
  /** at least 1 call made to the mock */
  def atLeastOne[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.atLeast(1))
  /** at least 2 calls made to the mock */
  def atLeastTwo[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.atLeast(2))
  /** at least 3 calls made to the mock */
  def atLeastThree[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.atLeast(3))
  /** at most n calls made to the mock */
  def atMost[T <: AnyRef](i: Int)(mock: =>T) = verify(mock, org.mockito.Mockito.atMost(i))
  /** at most 1 call made to the mock */
  def atMostOne[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.atMost(1))
  /** at most 2 calls made to the mock */
  def atMostTwo[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.atMost(2))
  /** at most 3 calls made to the mock */
  def atMostThree[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.atMost(3))
  /** no more calls made to the mock */
  def noMoreCallsTo[T <: AnyRef](mock: =>T) = mocker.verifyNoMoreInteractions(mock)
  /** implicit def supporting calls in order */
  implicit def toInOrderMode[T](calls: =>T) = new ToInOrderMode(calls)
  /** 
   * class defining a then method to declare that calls must be made in a specific order.
   * 
   * The orderedBy method can be used to declare the mock order if there are several mocks
   */
  class ToInOrderMode[T](calls: =>T) {
    def then[U](otherCalls: =>U) = {
      val newOrder = inOrder match {
        case Some(o) => Some(o)
        case None => Some(new InOrderImpl(new java.util.ArrayList[Object]))
      }
      val f = () => setTemporarily(inOrder, newOrder, (o:Option[InOrderImpl]) => inOrder = o) {
        calls
        otherCalls 
      }
      f() must new CallsMatcher
    }
    /** specify the mocks which are to be checked in order */
    def orderedBy(mocks: AnyRef*) = {
      setTemporarily(inOrder, Some(new InOrderImpl(java.util.Arrays.asList(mocks.toArray: _*) )), (o:Option[InOrderImpl]) => inOrder = o) {
        calls
      } 
    }
  }
}
/**
 * This trait provides functionalities to declare stub values on method calls.
 * 
 * Usage:<code>
 * 
 * mockedList.get(0) returns "one"
 * mockedList.get(0) returns ("one", "two")
 * mockedList.get(0) throws new Exception("unexpected")
 * mockedList.get(0) answers ( i => "value " + i.toString )
 * 
 * </code>
 * 
 * It is also possible to chain stubs like this: <code>
 * 
 * mockedList.get(0) returns "one" thenReturns "two"
 * mockedList.get(0) returns "one" thenThrows new Exception("unexpected now")
 * </code>
 */
trait MockitoStubs extends MocksCreation {
  /** delegate to MockitoMocker doAnswer with a MockAnswer object using the function f. */
  def doAnswer[T](f: Any => T) = mocker.doAnswer(new MockAnswer(f))
  
  /** @return an object supporting the stub methods. */
  implicit def theStubbed[T](c: =>T) = new Stubbed(c)

  /** 
   * This class provide stub methods like returns, throws and answers.
   * Internally it calls Mockito.when(mock call).thenReturn(returnValue)
   */
  class Stubbed	[T](c: =>T) {
    def returns(t: T, t2: T*): OngoingStubbing[T] = {
      if (t2.isEmpty) 
        mocker.when(c).thenReturn(t)
      else { // written like this to avoid a 2.8 compiler error: "cannot find class ClassManifest for element type of T*"
    	var stub = mocker.when(c).thenReturn(t)
    	t2 foreach { x =>
    		stub = stub.thenReturn(x)
    	}
    	stub
      }
    }
    def answers(function: Any => T) = mocker.when(c).thenAnswer(new MockAnswer(function))
    def throws[E <: Throwable](e: E*): OngoingStubbing[T] = {
      if (e.isEmpty) throw new java.lang.IllegalArgumentException("The parameter passed to throws must not be empty")
      var stub = mocker.when(c).thenThrow(e.head)
      e.drop(1) foreach { x =>
    	stub = stub.thenThrow(x)
      }
      stub
    }
  }
  /** @return an object allowing the chaining of returned values on doNothing calls. */
  implicit def aStubber(stub: =>Stubber) = new AStubber(stub)
  /** provide stub chain methods. */
  class AStubber[T](stub: =>Stubber) {
    def thenReturn[T](t: T) = stub.doReturn(t)
    def thenThrow[E <: Throwable](e: E) = stub.doThrow(e)
  }
  /** @return an object allowing the chaining of stub values. */
  implicit def anOngoingStubbing[T](stub: =>OngoingStubbing[T]) = new AnOngoingStubbing(stub)
  /** provide stub chain methods. */
  class AnOngoingStubbing[T](stub: =>OngoingStubbing[T]) {
    def thenReturns(t: T) = stub.thenReturn(t)
    def thenThrows[E <: Throwable](e: E) = stub.thenThrow(e)
  }
  /** allows to use a specs matcher to match parameters by encapsulating it as a Hamcrest matcher. */
  implicit def argThat[T](m: org.specs.matcher.Matcher[T]): T = org.mockito.Matchers.argThat(new org.specs.mock.HamcrestMatcherAdapter(m))
  /** allows to use a hamcrest matchers to match parameters. */
  def argThat[T](m: org.hamcrest.Matcher[T]): T = org.mockito.Matchers.argThat(m)

  /** 
   * This class is an implementation of the Answer interface allowing to pass functions as an answer.
   * 
   * It does a bit of work for the client:
   * 
   * // if the method has one parameter and the function also, the parameter is passed
   * mock.get(0) answers ( i => i.toString )
   * 
   * // if the method has one parameter and the function has two, the mock is passed as the second argument
   * mock.get(0) answers { (i, mock) => i.toString + " for mock " + mock.toString } 
   * 
   * Similarly a mocked method with no parameters can use a function with one parameter. In that case, the mock will be passed
   * mock.size answers { mock => mock.hashCode } 
   * 
   * In any other cases, if f is a function of 1 parameter, the array of the method parameters will be passed and if the function has
   * 2 parameters, the second one will be the mock.
   * 
   */
  class MockAnswer[T](function: Any => T) extends Answer[T] {
     def answer(invocation: InvocationOnMock): T = {
       val args = invocation.getArguments
       val mock = invocation.getMock
       if (args.size == 0) {
         function match {
           case f: Function0[_] => return f()
           case f: Function1[_,_] => return f(mock)
         }
       } else if (args.size == 1) {
         function match {
           case f: Function1[_, _] => return f(args(0))
         }
         function match {
           case f2: Function2[_, _, _] => return f2(args(0), mock)
         }
       } else {
         function match {
           case f: Function1[_, _] => return f(args)
         }
         function match {
           case f2: Function2[_, _, _] => return f2(args, mock)
         }
       }
     } 
  }
}
/**
 * shortcuts to standard Mockito functions
 */
trait MockitoFunctions extends TheMockitoMocker {
    /** delegate to MockitoMocker doReturn. */
  def doReturn[T](t: T) = mocker.doReturn(t)
  /** delegate to MockitoMocker doAnswer. */
  def doAnswer[T](a: Answer[T]) = mocker.doAnswer(a)
  /** delegate to MockitoMocker doThrow. */
  def doThrow[E <: Throwable](e: E) = mocker.doThrow(e)
  /** delegate to MockitoMocker doNothing. */
  def doNothing = mocker.doNothing
}
/**
 * Type-inference friendly Mockito matcher for 'any'
 */
trait MockitoMatchers {
  def any[T](implicit m: scala.reflect.ClassManifest[T]): T = org.mockito.Matchers.isA(m.erasure).asInstanceOf[T]
}
/** delegate to Mockito static methods with appropriate type inference. */
trait TheMockitoMocker {
  private[specs] val mocker = new org.mockito.MockitoMocker
}
