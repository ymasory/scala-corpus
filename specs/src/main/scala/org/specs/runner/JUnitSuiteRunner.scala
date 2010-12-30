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
package org.specs.runner;

import _root_.junit.framework._
import org.junit.runner.Description
import org.junit.runner.Description._
import org.junit.runner.manipulation._
import org.junit.runner.notification._
import org.specs.util.Stacktraces

/**
 * The JUnitSuiteRunner provides a JUnit4 annotation to run <code>JUnitSuite</code> test suites created from specifications
 * <code>klass</code> is a JUnitSuite which implements the junit.framework.Test interface
 */
class JUnitSuiteRunner(klass: java.lang.Class[T] forSome {type T <: Test}) extends org.junit.runner.Runner with Filterable with Sortable with TestDescription {

  /**
   * aggregated test representing the whole test suite
   */
  lazy val testSuite: Test = klass.newInstance

  /**
   * runs the test suite by passing a JUnit4 RunNotifier which is wrapped in a JUnit3 TestListener to be able to run JUnit3 tests
   */
  override def run(notifier: RunNotifier) = {
	val result = new TestResult
	result.addListener(createAdaptingListener(notifier));
	testSuite.run(result);
  }

  /**
   * adapt a JUnit4 RunNotifier with a JUnit3 TestListener
   */
  def createAdaptingListener(notifier: RunNotifier) = new OldTestClassAdaptingListener(notifier)

  /**
   * @return the description of the suite with the nested suites and tests
   */
  override def getDescription(): Description = {
    makeDescription(testSuite)
  }

  /**
   * nothing to filter
   */
  def filter(filter: Filter) = {}

  /**
   * nothing to sort
   */
  def sort(sorter: Sorter) = {}
}

/**
 * Common methods to create descriptions from a test or a test suite.
 * If the test is executed from Maven, its description can be simplified and not include
 * the test hashcode.
 */
trait TestDescription extends Stacktraces {
  /** return true if the current test is executed with Maven */
  lazy val isExecutedFromMaven = isExecutedFrom("org.apache.maven.surefire.Surefire.run")
  /** return true if the current test is executed with Intellij */
  lazy val isExecutedFromIntellij = isExecutedFrom("com.intellij.rt")
  /**
   * Describe a test including its hashCode instead of its class name. If the class name is included, some tests may
   * not render properly as there can only be one test with a given in a given class.
   * For specs it is different as there can be the same name in 2 different systems (and those systems will be represented by the
   * same JUnit class: ExampleTestCase).
   *
   * This uses the createSuiteDescription method from JUnit as it is the only way to create a Description object having
   * the required name.
   *
   * @return the description of the test with a name including its hashcode
   */
  def asDescription(test: Test) = {
    def getName(test: Test) = {
	  if (test.isInstanceOf[TestCase])
         test.asInstanceOf[TestCase].getName
      else
         test.toString
    }
    def testcode(test: Test) = {
      if (isExecutedFromMaven)
        ""
      else if (isExecutedFromIntellij) 
       "("+test.getClass.getName+")"
      else
       "("+test.hashCode+")"

    }
    createSuiteDescription(getName(test) + testcode(test), new UnusedAnnotation)
  }
  /**
   * This annotation is only used to avoid NPEs happening in the reporting
   * which can cause some issue with some IDEs (@see issue 146)
   */
  class UnusedAnnotation extends java.lang.annotation.Annotation {
    def annotationType = classOf[UnusedAnnotation]
  }
  /**
   * @return the description of the suite based on its name
   */
  def asDescription(ts: JUnitSuite) = {
    createSuiteDescription(if (ts.getName == null) "" else ts.getName, new UnusedAnnotation)
  }

  /**
   * create a Description from a TestCase or a JUnitSuite object
   */
  def makeDescription(test: Test): Description = {
	if (test.isInstanceOf[JUnitSuite]) {
  	  val ts = test.asInstanceOf[JUnitSuite];
      val description = asDescription(ts)
      for (suite <- ts.suites)
		description.addChild(makeDescription(suite))
      for (t <- ts.testCases)
        description.addChild(makeDescription(t))
	    description
    } 
    else if (test.isInstanceOf[TestCase])
	    asDescription(test.asInstanceOf[TestCase])
    else
	    createSuiteDescription(test.getClass())
  }
}

/**
 * This class listens for JUnit3 results (as a TestListener) and notifies a JUnit4 RunNotifier
 */
class OldTestClassAdaptingListener(notifier: RunNotifier)  extends TestListener with TestDescription {

  /**
   * Notifies the notifier of a test start
   */
  def startTest(test: Test) = notifier.fireTestStarted(asDescription(test))

  /**
   * Notifies the notifier of a test finish
   */
  def endTest(test: Test) = notifier.fireTestFinished(asDescription(test))

  /**
   * Notifies the notifier of a new test failure (an error in JUnit3 is a Failure in JUnit4)
   */
  def addError(test: Test, t: Throwable) = addNewFailure(test, t)

  /**
   * Notifies the notifier of a test failure.
   * specs specificity: if the failure is a SkippedAssertionError, then notify of a skip
   */
  def addFailure(test: Test, t: AssertionFailedError) = {
    t match {
      // unfortunately the skip message can not be included for display in a description object
      // otherwise the description created when running the test and the description creating when
      // parsing the whole suite for the first time will not match
      case skipped: SkippedAssertionError => {
        notifier.fireTestIgnored(makeDescription(test))
      }
      case _ => addNewFailure(test, t)
    }
  }
  private def addNewFailure(test: Test, t: Throwable) = notifier.fireTestFailure(new Failure(asDescription(test), t))
}

