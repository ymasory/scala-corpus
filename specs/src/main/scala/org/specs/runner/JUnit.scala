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
package org.specs.runner

import org.specs._
import org.specs.specification._
import _root_.junit.framework._
import _root_.org.junit.runner._
import org.specs.collection.JavaCollectionsConversion._
import org.specs.util.Stacktraces
import org.specs.execute._
import org.specs.util.Property

/**
 * The strategy for running Specifications with JUnit is as follow:<p>
 *
 * -the specifications are used to create JUnit3 TestSuite and TestCase objects
 * -however, since TestSuite is not an interface, a JUnitSuite trait is used to represent it (and the JUnitSuite uses a TestSuite internally to hold the tests)
 *   -a specification is represented as a JUnitSuite
 *   -a system under specification (sus) is represented as an ExamplesTestSuite <: JUnitSuite
 *   -an example is represented as an ExampleTestCase <: TestCase
 *
 * Then, the JUnitSuite which implements the junit.framework.Test interface can be run using
 * a custom JUnit4 annotation: JUnitSuiteRunner
 *
 * This JUnitSuite trait encapsulates a JUnit3 TestSuite to allow it being seen as a trait and not a class
 * The suite is initialized once whenever some information is required, like getName, or countTestCases
 */
@RunWith(classOf[JUnitSuiteRunner])
trait JUnitSuite extends Test {
  /** embedded JUnit3 TestSuite object */
  val testSuite = new TestSuite
  
  /**this variable is set to true if the suite has been initialized */
  private var initialized = false

  /**the init method is called before any "getter" method is called. Then it initializes the object if it hasn't been done before */
  def init = if (!initialized) { initialize; initialized = true }

  /**the initialize method should be provided to build the testSuite object by adding nested suites and test cases */
  def initialize

  /**run the embedded suite */
  def run(result: TestResult) = {
    init
    testSuite.run(result)
  }
  /**@return the name of the embedded suite */
  def getName = {
    init
    testSuite.getName
  }

  /**set the name of the embedded suite */
  def setName(n: java.lang.String) = testSuite.setName(n)

  /** add a new test to the embedded suite */
  def addTest(t: Test) = testSuite.addTest(t)

  /** @return the tests of the embedded suite (suites and test cases) */
  def tests: List[Test] = { init
    enumerationToList(testSuite.tests)
  }
  /** @return the number of tests of the embedded suite (suites and test cases) */
  def countTestCases: Int = { init
    tests.size
  }

  /**@return the test cases of the embedded suite */
  def testCases: List[Test] = {
    init
    for (tc <- tests; if (tc.isInstanceOf[TestCase]))
    yield tc.asInstanceOf[TestCase]
  }

  /**@return the test suites of the embedded suite */
  def suites = {
    init
    for (ts <- tests; if (ts.isInstanceOf[JUnitSuite] || ts.isInstanceOf[TestSuite]))
    yield ts.asInstanceOf[Test]
  }
}

/**
 * Extension of a JUnitSuite initializing the suite with one or more specifications
 */
trait JUnit extends JUnitSuite with Reporter with ExtendedJUnitSuite {
  def initialize = {
    if (filteredSpecs.size > 1)
      setName(this.getClass.getName.replaceAll("\\$", ""))
    else
      setName(filteredSpecs.headOption.map(_.description).getOrElse("no specs"))
    filteredSpecs foreach { specification =>
      specification.subSpecifications.foreach(s => addTest(new JUnit3(s)))
      specification.systems foreach { sus =>
        if (planOnly() || sus.examples.isEmpty)
          addTest(new ExampleTestCase(specification, sus, sus.description + " " + sus.verb))
        else if (sus.isAnonymous)
		  sus.examples foreach { e => asSuite(this).addExample(specification, e, "") }
        else {
          val examples = if (sus.hasOwnFailureOrErrors) sus :: sus.examples else sus.examples
          addTest(new ExamplesTestSuite(specification, sus.description + " " + sus.verb, examples, sus.ownSkipped.headOption))
        }
      }
    }
  }
}

/**
 * Concrete class providing specifications to be used as a JUnitSuite
 * @deprecated use JUnit4 instead as the JUnitSuite class runs with a JUnit4 runner
 */
@RunWith(classOf[JUnitSuiteRunner])
class JUnit3(val specifications: Specification*) extends JUnit {
  val specs: Seq[Specification] = specifications
}

/**
 * Concrete class providing specifications to be used as a JUnitSuite
 */
@RunWith(classOf[JUnitSuiteRunner])
class JUnit4(val specifications: Specification*) extends JUnit {
  val specs: Seq[Specification] = specifications
}


/**
 * A <code>ExamplesTestSuite</code> is a JUnitSuite reporting the results of
 * a System under test (sus) as a list of examples, represented by ExampleTestCase objects.
 * If an example has subExamples, they won't be shown as sub tests because that would necessitate to
 * run the example during the initialization time.
 */
class ExamplesTestSuite(specification: BaseSpecification, description: String, examples: Iterable[Examples], skipped: Option[Throwable]) extends JUnitSuite with ExtendedJUnitSuite {

  /**
   * create one TestCase per example
   */
  def initialize = {
    setName(description)
    examples foreach {  example => asSuite(this).addExample(specification, example, description) }
  }

  /**
   * runs the set of exemplesA <code>ExamplesTestSuite</code> is a JUnitSuite reporting the results of
   * a list of examples. If an example has subExamples, they are reported with a separate <code>ExamplesTestSuite</code>
   * If the list of examples is skipped (because the corresponding sus is skipped), then a SkippedAssertionError failure is sent
   * and the JUnitSuiteRunner will interpret this as an ignored test (this functionality wasn't available in JUnit3)
   */
  override def run(result: TestResult) = {
    if (!JUnitOptions.planOnly()) skipped.map(e => result.addFailure(this, new SkippedAssertionError(e)))
    super.run(result)
  }
}

trait ExtendedJUnitSuite extends Stacktraces {
  
  /**return true if the current test is executed with Maven */
  lazy val isExecutedFromMaven = isExecutedFrom("org.apache.maven.surefire.Surefire.run")

  implicit def asSuite(s: JUnitSuite) = new ExtendedSuite(s)
  
  class ExtendedSuite(s: JUnitSuite) {
    def addExample(specification: BaseSpecification, example: Examples, description: String) = {
	      // if the test is run with Maven the sus description is added to the example description for a better
      // description in the console
      val exampleDescription = (if (isExecutedFromMaven) (description + " ") else "") + example.description
      if (JUnitOptions.planOnly() || !example.hasSubExamples)
        s.addTest(new ExampleTestCase(specification, example, exampleDescription))
      else {
        if (!example.examples.isEmpty) {
          val examples = if (example.hasOwnFailureOrErrors) example :: example.examples else example.examples          
          s.addTest(new ExamplesTestSuite(specification, exampleDescription, examples, None))
        }
        else
          s.addTest(new ExampleTestCase(specification, example, exampleDescription))
      }
    }
  }
}
/**
 * A <code>ExampleTestCase</code> reports the result of an example<p>
 * It overrides the run method from <code>junit.framework.TestCase</code>
 * to add errors and failures to a <code>junit.framework.TestResult</code> object
 *
 * When possible, the description of the subexamples are added to their failures and skipped exceptions
 */
class ExampleTestCase(specification: BaseSpecification, val example: Examples, description: String) extends TestCase(description.replaceAll("\n", " ")) {
  override def run(result: TestResult) = {
    if (JUnitOptions.planOnly() || example.ownSkipped.isEmpty)
      result.startTest(this)
    def report(ex: Examples, context: String) = {
      ex.ownFailures foreach {
        failure: FailureException =>
                result.addFailure(this, new SpecAssertionFailedError(UserError(failure, context)))
      }
      ex.ownSkipped foreach {
        skipped: SkippedException =>
          result.addFailure(this, new SkippedAssertionError(UserError(skipped, context)))
      }
      ex.ownErrors foreach {
        error: Throwable =>
                result.addError(this, new SpecError(UserError(error, context)))
      }
    }
    if (!JUnitOptions.planOnly()) { 
      report(example, "")
      example.examples foreach {subExample => report(subExample, subExample.description + " -> ")}
    }
    if (specification.isTheLastExample(example))
    	specification.executeAfterSpec
    if (JUnitOptions.planOnly() || example.ownSkipped.isEmpty)
      result.endTest(this)
  }
}

case class UserError(t: Throwable, context: String) extends Throwable {
  setStackTrace(t.getStackTrace)
  override def getCause = t.getCause

  override def getMessage = {
    t match {
      case f: FailureException => context + t.getMessage
      case _ => t.getClass.getName + ": " + context + t.getMessage
    }
  }
}

/**
 * This class refines the <code>AssertionFailedError</code> from junit
 * and provides the stackTrace of an exception which occured during the specification execution
 */
class SpecAssertionFailedError(t: Throwable) extends AssertionFailedError(t.getMessage) {
  override def getStackTrace = t.getStackTrace
  override def getCause = t.getCause

  override def printStackTrace = t.printStackTrace

  override def printStackTrace(w: java.io.PrintStream) = t.printStackTrace(w)

  override def printStackTrace(w: java.io.PrintWriter) = t.printStackTrace(w)
}

/**
 * This class represents errors thrown in an example. It needs to be set as something
 * different from an AssertionFailedError so that tools like Ant can make the distinction between failures and errors
 */
class SpecError(t: Throwable) extends java.lang.Error(t.getMessage) {
  override def getStackTrace = t.getStackTrace

  override def getCause = t.getCause

  override def printStackTrace = t.printStackTrace

  override def printStackTrace(w: java.io.PrintStream) = t.printStackTrace(w)

  override def printStackTrace(w: java.io.PrintWriter) = t.printStackTrace(w)
}
class SkippedAssertionError(t: Throwable) extends SpecAssertionFailedError(t)

/**
 * This trait allows to pass system options to Tests and TestSuites
 */
object JUnitOptions {
  val planOnly = Property(System.getProperty("plan") != null)
}
