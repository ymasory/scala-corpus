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
import org.jmock.lib.legacy.ClassImposteriser
import org.jmock.lib.action._
import org.jmock.api._
import org.jmock.internal.State
import org.jmock.internal.StatePredicate
import java.util.Collection
import java.util.Iterator
import org.hamcrest._
import org.hamcrest.core._
import org.hamcrest.core.AnyOf._
import org.jmock._
import org.specs.specification._
import org.specs.util.Property
import org.specs.util.ExtendedThrowable._
import org.jmock.internal.matcher.MethodNameMatcher
import org.specs.collection.JavaCollectionsConversion._
import scala.reflect.ClassManifest
import org.specs.execute._


/** 
 * This object can be used to import and rename some functions in case of a conflict with some other trait.
 * For example the trait Suite in ScalaTest uses an expect method too
 */
object JMocker extends JMocker {
  def addExpectation = null
}

/** 
 * The JMocker trait is used to give access to the mocking functionalities of the JMock library 
 */
trait JMocker extends JMockerExampleLifeCycle with HamcrestMatchers with JMockActions with ExpectationsListener { outer =>

  /**
   * the mock method is used to create a mock object
   * Usage:<pre>val mocked = mock[InterfaceToMock]</pre><br/>
   * Classes can be mocked too, but the ClassImposterizer trait has to be added to the extensions
   * @returns the mocked object
   */
  def mock[T](implicit m : ClassManifest[T]): T = context.mock(m.erasure).asInstanceOf[T]

  /** 
   * mocks a class and give the resulting mock a name. jMock expects mocks of the same class to have different names
   * @returns the mocked object
   */
  def mockAs[T](name: String)(implicit m : ClassManifest[T]): T = context.mock(m.erasure, name).asInstanceOf[T]

  /** 
   * mocks a class and add expectations. Usage <code>willReturn(as[MyInterface]{m: MyInterface => one(m).method })<code>
   * @returns the mocked object and the evaluation of the block of expectations
   */
  def as[T](expects: Function1[T, Any])(implicit m : ClassManifest[T]): (T, Function1[T, Any]) = {
    (mock[T], expects) 
  }

  /** 
   * mocks a class and add expectations, specifying the mock with a name
   * @returns the mocked object and the evaluation of the block of expectations
   */
  def as[T](name: String)(expects: Function1[T, Any])(implicit m : ClassManifest[T]): (T, Function1[T, Any]) = {
    (mockAs[T](name), expects) 
  }
  /** 
   * mocks a class several times and add expectations for each mock. Usage <code>willReturn(as[MyInterface]),
   *   {m1: MyInterface => one(m1).method },
   *   {m2: MyInterface => one(m2).method },
   * )<code>
   * However, it is shorter to use <code>willReturnIterable:
   * one(workspace).projects willReturnIterable(as[Project], 
           {p: Project => one(p).name willReturn "p1" },
           {p: Project => one(p).name willReturn "p2" })<code>
   * @returns the mocked object and the evaluation of the block of expectations
   */
  def as[T](expects: Function1[T, Any]*)(implicit m : ClassManifest[T]) = {
    expects.toList.zipWithIndex map { x =>
      val (block, i) = x
      (mockAs[T](m.erasure.getName + "_" + i), block)
    }
  }

  /** 
   * the code block being evaluated should contain some expectations being added to the expectations variable
   * Then the expectations are "freezed" by calling the checking method on context and the actual system behaviour can
   * be exercised
   */
  def expect(block: => Any) = {
    val result = block.isExpectation
    context.checking(expectations)
    result
  }
  
  /** 
   * Adds call constraint method to integers to express the exactly, atLeast, atMost
   */
  implicit def intToCallConstraint(i: Int) = new IntCallConstraint(i)
  class IntCallConstraint(i: Int) {
    def of[T](a: T) = exactly(i).of(a)
    def atLeastOf[T](a: T) = atLeast(i).of(a)
    def atMostOf[T](a: T) = atMost(i).of(a)
  }

  /** 
   * Adds call constraint method to a range, to express the between constraint
   */
  implicit def RangeToCallConstraint(r: Range) = new RangeCallConstraint(r)
  class RangeCallConstraint(r: Range) {
    def of[T](a: T) = between(r.start, r.end).of(a)
  }
  
  /** expecting exactly one call to the mock */
  def one[T](m: T) = expectations.one(m)

  /** expecting exactly count calls to the mock */
  def exactly(count: Int) = expectations.exactly(count)
    
  /** expecting atLeast count calls to the mock */
  def atLeast(count: Int) = expectations.atLeast(count)
    
  /** expecting between minCount and maxCount (inclusive) calls to the mock */
  def between(minCount: Int, maxCount: Int) = expectations.between(minCount, maxCount)

  /** expecting at Most calls to the mock */
  def atMost(count: Int) = expectations.atMost(count)
    
  /** allowing any calls to mocks */
  def allowing[T](mockObjectMatcher: Matcher[T]) = expectations.allowing(mockObjectMatcher)
    
  /** allowing any calls to a mock with method names like the passed parameter, returning default values */
  def allowingMatch[T](mock: T, methodName: String) = expectations.allowing(new IsSame(mock)).method(withName(methodName))

  /** allowing any calls to any mock with method names like the passed parameter, returning default values */
  def allowingMatch(methodName: String) = expectations.allowing(anything).method(withName(methodName))

  /** allowing any calls to the mock */
  def allowing[T](mockObject: T) = expectations.allowing(mockObject)
    
  /** ignoring any calls to the mock, returning default values */
  def ignoring[T](mockObject: T) = expectations.ignoring(mockObject)
    
  /** ignoring any calls to the mock, returning default values */
  def ignoring[T](mockObjectMatcher: Matcher[T]) = expectations.ignoring(mockObjectMatcher)
    
  /** ignoring any calls to the mock with method names like the passed parameter, returning default values */
  def ignoringMatch(methodName: String) = expectations.ignoring(anything).method(withName(methodName))

  /** ignoring any calls to a mock with method names like the passed parameter, returning default values */
  def ignoringMatch[T](mock: T, methodName: String) = expectations.ignoring(new IsSame(mock)).method(withName(methodName))
 
  /** forbidding any calls to the mock */
  def never[T](mockObject: T) = expectations.never(mockObject)

  /** constraining the latest call expectation with a Hamcrest matcher */
  def `with`[T](matcher: Matcher[T]) = expectations.`with`(matcher)

  /** shortcut for expectations.`with`(new IsAnything[Int]) */
  def anyInt: Int = any[Int]

  /** shortcut for expectations.`with`(new IsAnything[Long]) */
  def anyLong: Long = any[Long]

  /** shortcut for expectations.`with`(new IsAnything[Short]) */
  def anyShort: Short = any[Short]

  /** shortcut for expectations.`with`(new IsAnything[Boolean]) */
  def anyBoolean: Boolean = any[Boolean]

  /** shortcut for expectations.`with`(new IsAnything[Float]) */
  def anyFloat: Float = any[Float]

  /** shortcut for expectations.`with`(new IsAnything[Double]) */
  def anyDouble: Double = any[Double]

  /** shortcut for expectations.`with`(new IsAnything[Char]) */
  def anyChar: Char = any[Char]

  /** shortcut for expectations.`with`(new IsAnything[Byte]) */
  def anyByte: Byte = any[Byte]

  /** shortcut for expectations.`with`(new IsAnything[String]) */
  def anyString: String = any[String] 

  /** shortcut for expectations.`with`(new IsAnything[T]) */
  def any[T](implicit m: ClassManifest[T]): T = expectations.`with`(anything[T])

  /** shortcut for expectations.`with`(new IsInstanceOf[T]) */
  def a[T](implicit m: ClassManifest[T]): T = {expectations.`with`(new IsInstanceOf(m.erasure)); null.asInstanceOf[T]}

  /** shortcut for expectations.`with`(new IsInstanceOf[T]) */
  def an[T](implicit m: ClassManifest[T]): T = {expectations.`with`(new IsInstanceOf(m.erasure)); null.asInstanceOf[T]}

  private def trueMatcher[T] = new org.hamcrest.TypeSafeMatcher[T]() {
    def matchesSafely(a: T) = true
    def describeTo(d: org.hamcrest.Description) = {}
  }

  /** shortcut for expectations.`with`(new IsNull[T]) */
  def aNull[T](implicit m: ClassManifest[T]): T  = {expectations.`with`(new IsNull[T]); null.asInstanceOf[T]}

  /** shortcut for expectations.`with`(new IsNot(IsNull[T])) */
  def aNonNull[T](implicit m: ClassManifest[T]): T  = {expectations.`with`(new IsNot(new IsNull[T])); null.asInstanceOf[T]}

  /** shortcut for expectations.`with`(new IsEqual[T](value)) */
  def equal[T](value: T)  = { expectations.`with`(new IsEqual[T](value)); value }

  /** shortcut for expectations.`with`(new IsSame[T](value)) */
  def same[T](value: T)  = {expectations.`with`(new IsSame[T](value)); value}
  
  /** @returns a specs matcher adapted into Hamcrest matcher */
  implicit def will[T](m: org.specs.matcher.Matcher[T]): T = {
    expectations.`with`(new HamcrestMatcherAdapter[T](m))
    null.asInstanceOf[T]
  }
  
  /** this method is used to avoid the use of the reserved Scala keyword 'with'. This is also consistent with the way to specify returned values */
  def will[T](m: org.hamcrest.Matcher[T]): T = {
    expectations.`with`(m)
    null.asInstanceOf[T]
  }

  /** allow the return value of the method to be more precisely specified with a JMock action, like a returnValue action */
  implicit def toAction[T](v: T) = new JMockAction(v)

  /** 
   * This class allows a block of code to be followed by some actions like returning a value or throwing an exception
   * When we wish to return a mock and define expectations at the same time:
   * <code>willReturn[MyInterface] { m1: MyInterface =>
   *    one(m1).method }
   * <code>
   * It is necessary to first create the mock, set it as a returned object through a ReturnValueAction
   * then to trigger the block containing the expectations for that mock
   */
  class JMockAction[T](v: T) {

    private[this] def wrap[T](collection: java.util.Collection[T]) = collection.toArray

    /** sets a value to be returned by the mock */
    def willReturnValue(result: T) = expectations.will(new ReturnValueAction(result))

    /** sets a value to be returned by the mock */
    def willReturn(result: T) = expectations.will(new ReturnValueAction(result))

    /** sets an action which will return a mock of type Class[T] and being specified by a function triggering expectations */
    def willReturn[T](f: Function1[T, Any])(implicit m: ClassManifest[T]): Unit = {
      val mocked: T = mock[T]
      expectations.will(new ReturnValueAction(mocked))
      f(mocked)
    }
    /** 
     * sets an action which will return a mock of type Class[T] and being specified by a function triggering expectations.
     * It is supposed to be used in conjunction with the <code>as</code> method which mocks an object and creates a function
     * defining the mock expectations
     */
    def willReturn(result: (T, Function1[T, Any])) = {
      expectations.will(new ReturnValueAction(result._1))
      result._2.apply(result._1)
    }

    def willReturnEach(values: T*) = {
      will(new ActionSequence((values map { x: T => returnValue(x) }).toArray:_*))
    }

   /** 
    * will return a list of mocks from the same class several times and add expectations for each mock. 
    * Usage <code>willReturnIterable(as[MyInterface](
    *   {m1: MyInterface => one(m1).method },
    *   {m2: MyInterface => one(m2).method },
    * )<code>
    * However, it is shorter to use <code>willReturnIterable:
    * one(workspace).projects willReturnIterable[Project]( 
            {p: Project => one(p).name willReturn "p1" },
            {p: Project => one(p).name willReturn "p2" })<code>
    */
    def willReturnIterable[S, T <: Iterable[S]](results: Function1[S, Any]*)(implicit m: ClassManifest[S]): Unit = {
      willReturn(results.toList.zipWithIndex map { x =>
        val (block, i) = x
        (mockAs[S](m.erasure.getName + "_" + i).asInstanceOf[S], block)
      })
    }
   /** 
    * will return a list of values of type S, and containing some associated blocks to set expectations for those values which may be mocks 
    * Usage <code>willReturnIterable(as[MyInterface]{m1: MyInterface => one(m1).method },
    *   as[MyInterface]{m2: MyInterface => one(m2).method })
    * <code>
    */
    def willReturnIterable[S, T <: Iterable[S]](results: (S, Function1[S, Any])*): Unit = willReturn(results.toList)

   /** 
    * will return a list of values of type S, and containing some associated blocks to set expectations for those values which may be mocks 
    * Usage <code>willReturnIterable(List(as[MyInterface]{m1: MyInterface => one(m1).method },
    *   as[MyInterface]{m2: MyInterface => one(m2).method }))
    * <code>
    */
    def willReturn[S, T <: Iterable[S]](results: List[(S, Function1[S, Any])]): Unit = {
      expectations.will(new ReturnValueAction(results.map(_._1).toList))
      results foreach { result =>
        result._2.apply(result._1)
      }
    }

    /** sets an exception to be thrown by the mock */
    def willThrow[X <: Throwable](t: X) = expectations.will(new ThrowAction(t))

    /** sets an exception to be thrown by the mock */
    def willThrow[X <: Throwable](implicit m: ClassManifest[X]) = expectations.will(new ThrowAction(m.erasure.newInstance.asInstanceOf[X]))

    /** set up a JMock action to be executed */
    def will(action: Action) = expectations.will(action)
    
    def willReturn[T](stored: CapturingParam[T]) = will(stored.value)

  }
  /** factory method to create a new capturing parameter */
  def capturingParam[T] = new CapturingParam[T]

  /** 
   * Capturing Parameters allow to capture the value of a parameter passed to a mock method.<p/>
   * The most frequent usage for this is to be able to return the parameter as the return value of the method.<p/>
   * Usage:<pre>
   * val s = capturingParam[String]
   * classOf[ToMock].expects(one(_).method(s.capture) willReturn s)
   * </pre>
   * It is also possible to use the <code>map</code> function to return a value of a different type:<pre> 
   * willReturn s.map(_.size)
   * </pre>
   * And the capturing parameter can still be checked for its validity using <code>must(specs matcher)</code>:<pre>
   * classOf[ToMock].expects(one(_).method(s.must(beMatching("h.*")).capture) willReturn s)
   * </pre>
   */
  class CapturingParam[T] extends org.hamcrest.TypeSafeMatcher[T]() {
    /** stores the captured value, to be able to return it later */
    var captured: T = _

    /** by default the captured parameter is the first parameter of the mocked method */
    private var parameterIndex  = 0

    /** option mapping function to apply before returning the value */
    private var function: Option[T => Any] = None

    /** optional matcher checking the captured value */
    private var matcher: Option[HamcrestMatcherAdapter[T]] = None

    /** the returned value as a ReturnValueAction object */
    def value = new ReturnValueAction() {
      override def invoke(i: Invocation) = function match {
        case None => i.getParameter(parameterIndex)
        case Some(f) => f(i.getParameter(parameterIndex).asInstanceOf[T]).asInstanceOf[java.lang.Object]
      }
    }

    /** this method stores the parameter value and apply the optional matcher */
    def matchesSafely(a: T): Boolean = { 
      captured = a
      matcher match {
        case None => true
        case Some(m) => m.matchesSafely(a) 
      } 
    }

    /** this describes the result of the optional matchers */
    def describeTo(desc: Description) = matcher.map(_.describeTo(desc))

    /** capture will add this as a new Matcher to expect */
    def capture = `with`(this)

    /** capture will add this as a new Matcher to expect, with the user-specified index of the parameter */
    def capture(i: Int) = { parameterIndex = i; `with`(this) }

    /** adds a function to use when returning the captured value */
    def map[S](f: T => S) = { function = Some(f); this }

    /** adds a matcher to use when checking the parameter */
    def must(m: org.specs.matcher.Matcher[T]) = { matcher = Some(HamcrestMatcherAdapter(m)); this }
  }


  /** set up a jMock action to be executed */
  def will(action: Action) = expectations.will(action)
    
  /** @returns a state with name <code>name<code>, creating a new one if necessary */
  def state(name: String) = context.states(name)

  /** specifies that a call can only occur following the specific condition on a state */
  def when(predicate: StatePredicate) = expectations.when(predicate)
    
  /** specifies that after a call, a State object will be in a specific state */
  def then(state: State) = expectations.then(state)

  /** allows any block of expectations to be followed by state actions and expectations */
  implicit def afterCall(v: Any) = new StateConstraint(v)

  /** this class allows any block of expectations to be followed by state actions and expectations */
  class StateConstraint(expectation: Any) {
    def set(state: State) = expectations.then(state)
    def when(predicate: StatePredicate) = expectations.when(predicate)
  }

  /** adds a constraint to the expectations to declare that an expected call must happen in sequence */
  def inSequence(sequence: Sequence) = expectations.inSequence(sequence)

  /** this class allows an expectation to declare that another expectation should follow */
  implicit def after(v: =>Any) = new InSequenceThen(v)

  /** this class allows an expectation to declare that another expectation should follow */
  class InSequenceThen(firstExpectation: =>Any) {
    val sequence = {
      val s = context.sequence("s")
      firstExpectation; inSequence(s)
      s
    }  
    def then(otherExpectation: Any) = {
      otherExpectation
      inSequence(sequence)
      this
    }
  }
  /** n
   * One liner function for expectations.<p>
   * Usage:<code>  
   *   expect[List[Int]] { one(_).size } { l =>
   *     l.size
   *   }
   *</code>
   */
  def expect[T](f: T => Any)(implicit m: ClassManifest[T]): ExpectBlock[T] = ExpectBlock(mock[T](m), f)
  case class ExpectBlock[T](mocked: T, f: T => Any) {
    
    def in(f2: T => Any) = isExpecting(mocked)(f)(f2)
    def mock: T = { expect { f(mocked) }; mocked }
  }
  private def isExpecting[T](m: =>T)(f: T => Any)(f2: T => Any): Any = {
    val lastExample = addExpectation
    var result: Any = null
    expect { f(m) }
    lastExample.specifyExample(result = f2(m))
    lastExample.executeExamples
    result
  }
  /** 
   * Extends Class objects with the one-liner <code>expects</code>
   * Usage:<code>  
   *   classOf[List[Int]].expect { one(_).size } { l =>
   *     l.size
   *   }
   *</code>
   */
  implicit def classToMock[T](c: Class[T]) = ClassToMock(c)
  /** 
   * Extends Class objects with the one-liner <code>expects</code>
   * Usage:<code>  
   *   classOf[List[Int]].expect { one(_).size } { l =>
   *     l.size
   *   }
   *</code>
   */
  case class ClassToMock[T](c: Class[T]) {
    private def block(f: T => Any)(implicit m: ClassManifest[T]) = ExpectBlock(mock[T](m), f)
    def expects(f: T => Any)(implicit m: ClassManifest[T]) = block(f)
    def expectsOne(f: T => Any)(implicit m: ClassManifest[T]) = block((m:T) => f(one(m)))
    def expectsSome(f: T => Any)(implicit m: ClassManifest[T]) = block((m:T) => f(atLeast(0).of(m)))
    def expectsAtLeastOne(f: T => Any)(implicit m: ClassManifest[T]) = block((m:T) => f(atLeast(1).of(m)))
    def expectsAtLeast(i: Int)(f: T => Any)(implicit m: ClassManifest[T]) = block((m:T) => f(atLeast(i).of(m)))
    def expectsAtMost(i: Int)(f: T => Any)(implicit m: ClassManifest[T]) = block((m:T) => f(atMost(i).of(m)))
    def expectsExactly(i: Int)(f: T => Any)(implicit m: ClassManifest[T]) = block((m:T) => f(exactly(i).of(m)))
    def expectsBetween(min: Int, max: Int)(f: T => Any)(implicit m: ClassManifest[T]): ExpectBlock[T] = block((m:T) => f(between(min, max).of(m)))
    def expectsBetween(range: Range)(f: T => Any)(implicit m: ClassManifest[T]): ExpectBlock[T] = expectsBetween(range.start, range.end)(f)
    def allows[S](mockObjectMatcher: Matcher[S])(implicit m: ClassManifest[T]): ExpectBlock[T] = block((m:T) => outer.allowing(mockObjectMatcher))
    /** allowing any calls to a mock with method names like the passed parameter, returning default values */
    def allowsMatch(methodName: String)(implicit m: ClassManifest[T]) = block((m:T) => outer.allowingMatch(m, methodName))
    /** allowing any calls to the mock */
    def isAllowed(implicit m: ClassManifest[T]) = block((m:T) => outer.allowing(m))
    /** ignoring any calls to the mock, returning default values */
    def isIgnored(implicit m: ClassManifest[T]) =  block((m:T) => outer.ignoring(m))
    /** ignoring any calls to the mock, returning default values */
    def ignores[S](mockObjectMatcher: Matcher[S])(implicit m: ClassManifest[T]) = block((m:T) => outer.ignoring(mockObjectMatcher))
    /** ignoring any calls to the mock with method names like the passed parameter, returning default values */
    def ignoresMatch(methodName: String)(implicit m: ClassManifest[T]) = block((m:T) => outer.ignoringMatch(m, methodName))(m)
    /** forbidding any calls to the mock */
    def neverExpects(f: T => Any)(implicit m: ClassManifest[T]) = block((m:T) => f(outer.never(m)))(m)

  }
}
/** Adapter class to use specs matchers as Hamcrest matchers */
case class HamcrestMatcherAdapter[T](m: org.specs.matcher.Matcher[T]) extends org.hamcrest.TypeSafeMatcher[T] {
   var result = (true, "", "")
   def matchesSafely(item: T) = {
     result = m.apply(item)
     result._1
   }
   def describeTo(description: Description) = {
     description.appendText(result._2)
   }
}

/**
 * This trait provide methods to build Hamcrest matchers. It actually contains only 2 methods since all Hamcrest matchers can
 * be created by static methods in the Hamcrest library classes
 */
trait HamcrestMatchers {
  /** shortcut for new IsAnything[T] */
  def anything[T] = new IsAnything[T]

  /** @returns a MethodNameMatcher to use in conjunction with allowing(mock) */
  def withName(nameRegex: String) = new MethodNameMatcher(nameRegex)
}

/**
 * This trait provide methods to build jMock actions
 */
trait JMockActions {
  /** action returning a value */
  def returnValue[T](result: T) = new ReturnValueAction(result)

  /** action throwing an exception of type T*/
  def throwEx[T <: Throwable](t: T) = new ThrowAction(t)
    
  /** action executing several other actions */
  def doAll(actions: Action*) = new DoAllAction(actions.toArray:_*)
    
  /** @returns a sequence of actions where the first action is executed after the first call, the second action
   * after the second call, and so on */
  def onConsecutiveCalls(actions: Action*) = new ActionSequence(actions.toArray:_*)
}

/** 
 * This trait defines when a jMock context will be created and expectations checked.
 * The context and expectations are always created at the beginning of an Example, then checked just after the test has been executed. Executing a test can also trigger an ExpectationError (from the jMock library). This error will be transformed into a FailureException
 */
trait JMockerExampleLifeCycle extends LifeCycle with JMockerContext {

  /** 
   * An expectation error may be thrown during the execution of a test
   * In that case, it is transformed to a failure exception
   */
  override def executeExpectations(ex: Examples, t: => Any) = {
    try { 
      super.executeExpectations(ex, t) 
    } 
    catch { 
      case e: ExpectationError => throw createFailure(e) 
    }
  }

  /** 
   * After a test the context is verified. If some more expectations are not met an ExpectationError is thrown
   * In that case, it is transformed to a failure exception
   */
  override def afterExpectations(ex: Examples) = {
    try {
      context.assertIsSatisfied
    } catch {
      case e: ExpectationError => {restart; throw createFailure(e)}
    }
    super.afterExpectations(ex)
  }
  
  private[this] def createFailure(e: ExpectationError) = FailureException(e.toString)
}

/** 
 * This trait contains the jMock Mockery object and current expectations
 */
trait JMockerContext extends Imposterizer {
  /** the context holds the mocks, the expectations so is able to get the calls and check them */
  var context = createMockery

  /** call expectations with optional constraints on the number of calls, on parameters, return values,... */
  var expectations = new Expectations()
  
  /** creates a new context and a new Expectations object */
  def restart = { context = createMockery; expectations = new Expectations() }

  /** 
   * This method can be used to check the context. It will always restart the context, even if there is an ExpectationError
   * In that case, it is transformed to a failure exception
   */
  def checkContext = {
    try {
      context.assertIsSatisfied
    } catch {
      case e: ExpectationError => {restart; throw e}
    }
    restart
  }
}


/** 
 * This trait allows to mock classes instead of interfaces only. This will require the cglib and objenesis libraries on the path. 
 */
trait ClassMocker extends Imposterizer {
  override def newMockery = new Mockery() { setImposteriser(ClassImposteriser.INSTANCE) }
}

/** 
 * Naming scheme adding a number to each anonymous mock 
 */
class CountingNamingScheme extends org.jmock.api.MockObjectNamingScheme {
  private val camelCaseNamingScheme = new org.jmock.lib.CamelCaseNamingScheme
  private var counter = 1
  def defaultNameFor(typeToMock: Class[_]) = { 
    var name = camelCaseNamingScheme.defaultNameFor(typeToMock)
    if (counter > 1) {
      name = (name + " " + counter)
    }
    counter += 1; 
    name
  }
}
/** 
 * Abstract trait for creating a mocking context. By default, only allows to mock interfaces 
 */
trait Imposterizer {
  def createMockery = { val mockery = newMockery; mockery.setNamingScheme(new CountingNamingScheme); mockery }
  def newMockery = new Mockery
}
