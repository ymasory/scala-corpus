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
package org.specs
import org.specs.util._
import scala.xml._
import org.specs.matcher._
import scala.collection.mutable._
import org.specs.runner._
import org.specs.matcher.MatcherUtils._
import org.specs.SpecUtils._
import org.specs.specification._
import org.specs.util.ExtendedThrowable._
import org.specs.execute._

/**
 * This class is the main class for declaring a new specification<br>
 * In the context of a specification, you can:<ul>
 * <li>declare nested specifications
 * <li>define systems under test
 * <li>specify examples and expectations</ul>
 * Usage: <code>object mySpec extends Specification</code>
 * <p>
 * A specification is "executed" when it is constructed, then the failures and errors can
 * be collected with the corresponding methods
 *
 */
abstract class Specification extends BaseSpecification with Expectations with FailOrSkip with Console with SpecsFilter 
               with Contexts {

  /** A specification is its own specs holder. */
  val specs = List(this)

  /**
   * Alternate constructor with the name of the specification
   */
  def this(n: String) = { this(); name = n; description = n; this }

  /**
   * Alternate constructor with subspecifications
   */
  def this(subspecs: Specification*) = { this(); subSpecifications = subspecs.toList; this }

  /** @return a description of this specification with all its systems (used for the ConsoleReporter) */
  def pretty = description + systems.foldLeft("")(_ + _.pretty(addSpace("\n")))

  /** syntactic sugar to create a list of specifications starting from this one */
  def ::(s: Specification) = List(s, this)

  /** implementation of the error method for a Specification */
  def error(msg: String) = Predef.error(msg)

  /**
   * when setting the arguments on this specification, make sure that the tags are set accordingly
   */
  override def args_=(a: Array[String]) = {
    super.args_=(a)
    super.setTags(List(this))
  }
}

/**
 * This trait gives some flexibility when mixing in the ScalaTest trait because it uses the same method names
 */
 trait FailOrSkip {
   /**
    * Convenience method: adds a new failure to the latest example<br>
    * Usage: <code>fail("this code should fail anyway")</code>
    */
   def fail(m: String) = FailureException(m).hideCallerAndThrow(this)

   /**
    * Convenience method: adds a new failure to the latest example. The failure message is "failure"<br>
    * Usage: <code>fail</code>
    */
   def fail(): Nothing = fail("failure")

   /**
    * Convenience method: adds a new skippedException to the latest example<br>
    * Usage: <code>skip("this example should be skipped")</code>
    */
   def skip(m: String) = SkippedException(m).hideCallerAndThrow("org.specs.Specification")

 }

/**
 * This trait can be used to access Matchers functionalities outside a Specification.
 * For example like this:<code>
 *
 *  trait Functions extends Expectations {
 *    def bar(name: String) = name.length < 4 mustBe true
 *  }
 *  object Foo extends Specification with Functions {
 *    bar("Foo")
 *    bar("FooFoo")
 *  }
 * </code>
 *
 */
trait Expectations extends Matchers with OrResults with ExpectableFactory with DetailedFailures  
/**
 * This trait can be reused in any test based framework to access Matchers functionalities
 */
trait SpecsMatchers extends Expectations with DefaultExampleExpectationsListener


/** utility object to indent a string with 2 spaces */
object SpecUtils {
  /** @return <code>s + "  "</code> */
  def addSpace(s: String) = s + "  "
}

