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

import org.specs.io.mock.MockOutput
import org.specs.collection.JavaCollectionsConversion._
import org.specs.runner._
import org.specs.Sugar._
import _root_.junit.framework._
import org.junit.runner.notification.RunNotifier
import org.junit.runner.Description
import org.specs.specification._
import org.specs._

class junitTestSuiteSpec extends SpecificationWithJUnit {
  "A junit test suite execution" should {
	"trigger the afterSpec actions after all the examples if the specification is sequential" in {
	  val sequential = new SequentialSpecification
      val result = new TestResult
      sequential.run(result)
      sequential.messages must containInOrder("ex2", "afterSpec")
    }
  }
  "A junit test suite for a composite specification" should {
    "create one test suite per specification" in {
      val S1 = new Specification { 1 must_== 1 }
      val S2 = new Specification { 1 must_== 1 }
      val Composite = new Specification { "this composite spec" isSpecifiedBy (S1, S2) }

      makeRunners(Composite) foreach { r =>
        r.suites must have size(2)
      }
    }
  }
  "A junit test suite" should {
    "create high-level examples for an anonymous sus" in {
      object S1 extends Specification {
        "ex1" in { 1 must_== 1 }
        "ex2" in { 1 must_== 1 }
      }
      makeRunners(S1) foreach { r =>
        r.suites must be empty;
	    r.tests must have size(2)
      }
    }
    "create one test suite per sus" in {
      val S1 = new Specification {
        "sus1" should { "ex" in { 1 must_== 1 } }
        "sus2" should { "ex" in { 1 must_== 1 } }
      }
      makeRunners(S1) foreach { r =>
        r.suites.map(_.asInstanceOf[JUnitSuite].getName) must_== List("sus1 should", "sus2 should")
      }
    }
    "create one test case per example" in {
      val S1 = new Specification {
        "sus1" should { "ex1" in { 1 must_== 1 }; "ex2" in { 1 must_== 1 }}
      }
      makeRunners(S1) foreach { r =>
        val test1 = r.suites.flatMap(_.asInstanceOf[JUnitSuite].testCases).head
        val test2 = r.suites.flatMap(_.asInstanceOf[JUnitSuite].testCases).last
        test1.toString must include("ex1")
        test2.toString must include("ex2")
      }
    }
    "report a failure with a stacktrace pointing to the expectation causing it in the executed specification" in {
      val result = new TestResult
      suite(that.isKo).run(result)
      result.failures verifies(_.hasMoreElements)
      val failure = result.failures.nextElement.asInstanceOf[TestFailure]
      failure.exceptionMessage must_== "'ok' is not the same as 'first failure'"
      failure.trace.split("\n")(0) must include(failure.exceptionMessage)
      failure.trace.split("\n")(1) must be matching("Expectations") and be matching("consoleReporterSpec.scala:\\d")
    }
    "report an error with a stacktrace indicating the location of the error in the specification" in {
      val result = new TestResult
      suite(that.throwsAnException).run(result)
      result.errors verifies(_.hasMoreElements)
      val error = result.errors.nextElement.asInstanceOf[TestFailure]
      error.exceptionMessage must_== "java.lang.Exception: new Error"
      error.trace.split("\n")(0) must include(error.exceptionMessage)
      error.trace.split("\n")(1) must (beMatching("Expectations") and beMatching("consoleReporterSpec.scala:\\d"))
    }
    "report an error with the cause of the error" in {
      val result = new TestResult
      suite(that.throwsAnExceptionWithACause).run(result)
      val error = result.errors.nextElement
      error.thrownException.getCause mustNot be(null)
    }
    "report a skipped test" in {
      val result = new TestResult
      val listener = new RunNotifier {
        var desc: Option[Description] = None
        override def fireTestIgnored(d: Description) = desc = Some(d)
      }
      result.addListener(new OldTestClassAdaptingListener(listener))
      suite(that.isSkipped).run(result)
      listener.desc must beSome[Description]
    }
    "create one test per nested example whatever the depth" in {
      val specsWithNestedExamples = new Specification {
        "sus1" should { 
          "ex" in { 
            "first nested" in { 
              "second nested" in { 
                1 must_== 1
              }
            }
          } 
        }
      }
      makeRunners(specsWithNestedExamples) foreach { r =>
        r.suites(0).asInstanceOf[JUnitSuite].
          suites(0).asInstanceOf[JUnitSuite].
          suites(0).asInstanceOf[JUnitSuite].
          tests(0).toString must include("second nested")
      }
    }
  }
  def suite(behaviours: that.Value*) = new JUnit4(new SimpleSpecification(behaviours.toList))
  def makeRunners(spec: Specification) = {
    object R1 extends JUnit4(spec)
    object R2 extends Runner(spec) with JUnit
    List(R1, R2)
  }
  "An example test suite" should {
    "append the description of the sus to the example description if the runner is Maven" in {
      val s = new Specification {
        val e = "be ok" in { 1 must_== 1 }
      }
      val suite = new ExamplesTestSuite(s, "it should", List(s.e), None) {
        override lazy val isExecutedFromMaven = true
      }
      suite.tests.head.toString aka "the example description" must include("it should be ok")
    }
  }
  "A test description" should {
    val description = new TestDescription() {
      override lazy val isExecutedFromMaven = false
    }
    import _root_.junit.framework._
    class ATest() extends TestCase("name")
    "append the hashcode of the test to its description if not run from Maven or Intellij" in {
      description.asDescription(new ATest()).toString must beMatching(".*\\(.*\\)")
    }
    "not have null annotations" in {
      description.asDescription(new ATest()).getAnnotations() must not be(null)
    }
  }
  "A sus" should {
    "report its exceptions if any" in {
      val messages = new SpecificationWithJUnit with MockOutput {
        "a failing system"  should { error("bad"); "an example" in { 1 must_== 1 } }
      }.reportSpecs.messages 
      messages must containMatch("bad")
    }
  }
}
class SimpleSpecification(behaviours: List[(that.Value)]) extends TestSpecification {
  "A specification" should {
    "have example 1 ok" in {
      expectations(behaviours) foreach {_.apply}
    }
  }
}
class SequentialSpecification extends SpecificationWithJUnit with MockOutput {
  setSequential()
  shareVariables()
  doAfterSpec(println("afterSpec"))
  "sus1" should {
	println("sus1");
	"ex1" in { println("ex1"); 1 must_== 1 }
	"ex2" in { println("ex2"); 1 must_== 1 }
  }
}
