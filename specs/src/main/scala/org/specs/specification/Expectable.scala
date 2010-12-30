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
import org.specs.matcher._
import org.specs.matcher.Matchers._
import org.specs.util.ExtendedThrowable._
import org.specs.util.Property
import org.specs.Sugar._
import org.specs.execute._

/**
 * An expectable is an object supporting the execution of expectations through matchers.<pre>
 *   thisExpectable must passMatcher
 * </pre>
 * It can be optionally related to an example when created for an anonymous example.
 * Otherwise it just fires a FailureException when failing:<pre>
 * object spec extends Specification {
 *   // is automatically related to an anonymous example
 *   // it will be executed only once the example is executed
 *   // @see org.specs.specification.ExpectableFactory
 *   // @see org.specs.specification.ExampleExpectationsListener
 *   1 must_== 1
 *
 *   // in that case, no example is set but during the execution of the "in" part
 *   // the failure exception will be caught by the example and stored
 *   "this example fails" in { 1 must_== 0 }
 * }
 * object test extends SpecsMatchers {
 *   // this expectable is not related to any example and executes right away throwing an exception if failing
 *   1 must_== 1
 * }
 * </pre>
 */
class Expectable[T](expectableValue: => T) {
  /** related example. */
  private var example: Option[Examples] = None
  /** function used to display success values as a result of a match. By default nothing is displayed. */
  private var successValueToString: SuccessValue => String = s => ""
  /** the listener will be called for every match to register a new Expectation. */
  private var expectationsListener: Option[ExampleExpectationsListener] = None
  /** the function creating failure exceptions. */
  private var failureFactory: FailureFactory = new SpecsFailureFactory {}
  /**
   * stores a precise description of the thing being expected.
   * This description is meant to be passed to the matcher for better failure reporting.
   */
  protected var description: Option[String] = None
  /** state variable storing how the next matcher should be applied, negated or not */
  private var nextMatcherMustBeNegated = false
  /** set the state variable declaring that the next match should be negated, in the case of an xor combination to force a fail for example */
  private[specification] def nextSignificantMatchMustBeNegated() = { nextMatcherMustBeNegated = true; this }
  /** previous previous messages in the case of or-ed matchers*/
  private var matchMessages: List[String] = Nil
  /** add a previous message in the case of or-ed matchers*/
  protected def addMatchMessage(m: String): this.type = { matchMessages = matchMessages ::: List(m); this }
  /**
   * Apply a matcher for this expectable value.
   *
   * Execute the matcher directly or add it to its related example for execution.
   * It either throws a FailureException or return a SuccessValue object.
   *
   * The expectation listener gets notified of a new expectation with a fresh copy of this expectable.
   * The matcher gets
   */
  private[specification] def applyMatcher(m: => Matcher[T]): Result[T] = {
    val failureTemplate = FailureException("")
    val matcher = m
    matcher match {
      case okMatcher: org.specs.matcher.OkWordMatcher[_] => return new Result(this, successValueToString)
      case notMatcher: org.specs.matcher.NotMatcher[_] => { 
        nextMatcherMustBeNegated = true
        return new Result(this, successValueToString) 
      }
      case _ => expectationsListener.map(_.addExpectation(Some(this)))
    }

    def executeMatch = {
      matcher.setDescription(description)
      val (result, okMessage, koMessage) = {
        if (nextMatcherMustBeNegated) {
          nextMatcherMustBeNegated = false
          matcher.not.apply(expectableValue)
        } 
        else 
          matcher.apply(expectableValue)
      }
      result match {
        case false => {
          addMatchMessage(koMessage)
          failureFactory.createFailure(makeFailureMessage, 
                                         new Result(this, successValueToString)).throwWithStackTraceOf(failureTemplate.removeTracesWhileNameMatches("(Expectable.scala|Matchers.scala)"))
        }
        case _ => {
          addMatchMessage(okMessage)
          new Result(this, successValueToString)
        }
      }
    }
    example match {
      case None => executeMatch
      case Some(e) => {
        var res = new Result(this, successValueToString)
        e.specifyExample(res = executeMatch)
        res
      }
    }
  }
  /** create the failure message from all previous match messages */
  private def makeFailureMessage: String = {
    if (matchMessages.size == 0) ""
    else if (matchMessages.size == 1) matchMessages(0)
    else matchMessages.mkString(" and ")
  }
  /**
   * Set a specific example to hold the results of this matcher
   */
  def setExample[T](ex: Examples) = example = Some(ex)

  /** setter for the expectation listener. */
  def setExpectationsListener(listener: ExampleExpectationsListener): this.type = {
    expectationsListener = Some(listener)
    this
  }

  /** setter for the failure factory. */
  def setFailureFactory(factory: FailureFactory): this.type = {
    failureFactory = factory
    this
  }
  /**
   * Set a new function to render success values
   */
  def setSuccessValueToString(f: SuccessValue =>String) = successValueToString = f
  
  def valueProperty: Property[T] = Property(expectableValue)
}
/**
 * The Expect class adds matcher methods to objects which are being specified<br>
 * Usage: <code>new Expect(value, example) must beMatching(otherValue)</code><p>
 *
 * An assert is created with its parent <code>Example</code> in order to register failures
 * and errors if a matcher is not ok
 *
 */
class Expectation[T](value: => T) extends Expectable[T](value) {

  override def toString() = value.toString
  def createClone = new Expectation(value)

  /** set a better description on the value. */
  def aka(desc: String): this.type = { description = Some(desc); this }

  /**
   * applies a matcher to the current value and throw a failure is the result is not true
   */
  def must(m: => Matcher[T]) = applyMatcher(m)
  /**
   * applies the negation of a matcher
   */
  def mustNot(m: => Matcher[T]) =  must(m.not)

  /** alias for <code>must verify(f)</code>  */
  def mustVerify(f: T => Boolean) = must(verify(f))

  /** alias for <code>mustVerify(f)</code>  */
  def verifies(f: T => Boolean) = mustVerify(f)

  /** alias for <code>must be(other)</code>  */
  def mustBe(otherValue: Any) = must(be(otherValue))

  /** alias for <code>must be(other)</code>  */
  def mustEq(otherValue: Any) = must(be(otherValue))

  /** alias for <code>must notEq(other)</code>  */
  def mustNotBe(otherValue: Any) = must(notEq(otherValue))

  /** alias for <code>must notEq(other)</code>  */
  def mustNotEq(otherValue: Any) = mustNotBe(otherValue)

  /** alias for <code>must is_!=(other)</code>  */
  def must_!=(otherValue: Any)(implicit details: Detailed) = must(is_!=(otherValue)(details))

  /** alias for <code>must is_==(other)</code>  */
  def must_==(otherValue: Any)(implicit details: Detailed) = {
    must(is_==(otherValue)(details))
  }

  /** alias for <code>must is_==(other)</code>  */
  def mustEqual(otherValue: Any)(implicit details: Detailed) = must(is_==(otherValue)(details))
}

/** Specialized expectable class with string matchers aliases */
class StringExpectable[A <: String](value: => A) extends Expectable[A](value) {

  def createClone = new StringExpectable(value)
  /** alias for <code>must(beMatching(a))</code> */
  def mustMatch(a: String) = applyMatcher(beMatching(a))

  /** alias for <code>must(not(beMatching(a)))</code> */
  def mustNotMatch(a: String) = applyMatcher(not(beMatching(a)))

  /** alias for <code>must(beEqualToIgnoringCase(a))</code> */
  def must_==/(a: String) = applyMatcher(beEqualToIgnoringCase(a))

  /** alias for <code>must(notBeEqualToIgnoringCase(a))</code> */
  def must_!=/(a: String) = applyMatcher(notBeEqualToIgnoringCase(a))
}
/** Specialized expectable class with iterable matchers aliases */
class IterableExpectable[I <: AnyRef](value: =>Iterable[I]) extends Expectable[Iterable[I]](value) {
  def createClone = new IterableExpectable(value)

  /** alias for <code>must(exist(function(_))</code> */
  def mustHave(function: I => Boolean) = applyMatcher(have(function((_:I))))

  /** alias for <code>must(notExist(function(_))</code> */
  def mustNotHave(function: I => Boolean) = applyMatcher(notHave(function((_:I))))

  /**
   * alias for <code>must(exist(function(_))</code>
   * @deprecated use mustHave instead
   */
  def mustExist(function: I => Boolean) = mustHave(function)

  /**
   * alias for <code>must(notExist(function(_))</code>
   * @deprecated use mustNotHave instead
   */
  def mustNotExist(function: I => Boolean) = mustNotHave(function)

  /** alias for <code>must(contain(a))</code> */
  def mustContain(elem: I) = applyMatcher(contain(elem))

  /** alias for <code>must(notContain(a))</code> */
  def mustNotContain(elem: I) = applyMatcher(notContain(elem))
}
/** Specialized expectable class with iterable[String] matchers aliases */
class IterableStringExpectable(value: =>Iterable[String]) extends Expectable[Iterable[String]](value) {
  def createClone = new IterableStringExpectable(value)

  /** alias for <code>must(containMatch(pattern))</code> */
  def mustContainMatch(pattern: String) = applyMatcher(containMatch(pattern))

  /** alias for <code>must(notContainMatch(pattern))</code> */
  def mustNotContainMatch(pattern: String) = applyMatcher(notContainMatch(pattern))

  /** alias for <code>must(containMatch(pattern))</code> */
  def mustHaveMatch(pattern: String) = applyMatcher(containMatch(pattern))

  /** alias for <code>must(notContainMatch(pattern))</code> */
  def mustNotHaveMatch(pattern: String) = applyMatcher(notContainMatch(pattern))

  /**
   * alias for <code>must(containMatch(pattern))</code>
   * @deprecated: use mustContainMatch or mustHaveMatch instead
   */
  def mustExistMatch(pattern: String) = applyMatcher(containMatch(pattern))

  /**
   * alias for <code>must(notContainMatch(pattern))</code>
   * @deprecated: use mustNotContainMatch or mustNotHaveMatch instead
   */
  def mustNotExistMatch(pattern: String) = applyMatcher(notContainMatch(pattern))
}
/**
 * By default the result value of an expectable expression doesn't output anything when
 * toString is called.
 */
/**
 * This trait transforms SuccessValue objects to a Boolean value if it is necessary, for example in
 * ScalaCheck properties.
 */
trait SuccessValues {

  /** transforms a SuccessValue to a boolean */
  implicit def successValueToBoolean(s: SuccessValue) = true

  /** by default a SuccessValue is "silent" */
  def successValueToString(s: SuccessValue) = ""

  /** 
   * this implicit def allows a result to be or-ed with a matcher.
   */
}
trait OrResults {
  implicit def toOrResult[T](r: =>Result[T]) = new OrResult(r)
  /** 
   * this class allows a result to be or-ed with a matcher so that
   * if the result fails, the next matcher will still be tried 
   */
  class OrResult[T](r: =>Result[T]) {
    def or(m: => Matcher[T]): Result[T] = {
      var result: Result[T] = null
      try { 
        result = r
        result.setAlreadyOk()
      } catch {
        case f: HasResult[_] => return f.asInstanceOf[HasResult[T]].result.matchWith(m)
        case t => throw t
      }
      result
    }
    def xor(m: => Matcher[T]): Result[T] = {
      var result: Result[T] = null
      try { 
        result = r
        // if the first result is ok, then the next matcher must fail
        try { 
          result.nextSignificantMatchMustFail().matchWith(m)
        } catch {
          case f: HasResult[_] => return f.asInstanceOf[HasResult[T]].result.matchWith(m)
          case t => throw t
        }
      } catch {
        case f: HasResult[_] => return f.asInstanceOf[HasResult[T]].result.matchWith(m)
        case t => throw t
      }
      result
    }
  }
}
/** 
 * Special failure exception carrying a Result object, carrying an Expectable.
 * This Exception is necessary to handle the "OR" case "value must be equalTo(bad) or be equalTo(good)"
 * where the first match is not ok.
 */
class FailureExceptionWithResult[T](m: String, @transient val result: Result[T]) extends FailureException(m) with HasResult[T]
/** value returned by an expectable whose string representation can vary. */
trait SuccessValue

