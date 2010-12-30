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
import org.spex._
import org.specs.io.mock._
import org.specs.Sugar
import org.junit.runner.notification._

class reporterPlanSpec extends Specification with Sugar {
  "A console reporter with the -plan option" should {
    "show the -plan options when displaying the help" in {
      help must containMatch("-plan | --planOnly")
    }
    "show the -plan option description when displaying the help" in {
      help must containMatch("only display") and containMatch("without executing")
    }
  }
  include(ReporterPlan("console reporter", consoleReporter), 
          ReporterPlan("xml reporter", xmlReporter),
          ReporterPlan("junit reporter", junitReporter),
          ReporterPlan("ScalaTest reporter", scalaTestReporter))
  
  case class ReporterPlan(n: String, reporter: {def plan: String; def expectations: String}) extends Specification(n) with Sugar {
    "A "+n+" with the -plan option, when reporting the specification" should {
      "not execute examples, thus show 0 expectations" in {
        reporter.plan must not be matching(reporter.expectations)
      }
      "display the sus descriptions" in {
        reporter.plan must include("it")
      }
      "display the first level examples" in {
        reporter.plan must include("do this")
      }
      "not display the second level examples" in {
        reporter.plan must not include("subex1")
      }
    }
  }
  val s = new TestSpecification {
   "it" should { 
      "do this" in {1 must_== 1} 
      "do that" in {1 must_== 1}
    }
    "it also" should {
      "do these" >> {
        "subex1" in { 1 must_== 1 }
      }
      
    }
  } 
  class TestSpecification extends org.specs.Specification with MockOutput with ScalaTest {
    def help = displayHelp
  }
  object consoleReporter {
    def plan: String = {
      s.args = Array("-plan")
      s.reportSpecs
      s.messages.toList.mkString("\n")
    }
    def expectations: String = "[1-9] expectation"
  }
  val xml = new XmlRunner(s, "target") with MockFileSystem with MockOutput
  object xmlReporter {
    def plan: String = { 
      xml.args = Array("-plan")
      xml.reportSpecs
      xml.files.valuesIterator.next
    }
    def expectations = "expectations=\"[1-9]\""
  }
  val junit = new JUnit4(s)
  object result extends _root_.junit.framework.TestResult {
    var messages = ""
    override def startTest(test: _root_.junit.framework.Test) = {
      test match {
        case e: ExampleTestCase => messages = messages + e.example.parent.get.toString + ":" + test.toString + "\n" 
      }
    }
    def output = messages
  } 
  object junitReporter {
    def plan: String = {
      JUnitOptions.planOnly(true)
      junit.run(result)
      result.output
    }
    def expectations: String = "[1-9] expectation"
  }

  object scalaTestReporter {
    class MockReporter extends org.scalatest.Reporter {
      var messages = ""
      def apply(e: org.scalatest.events.Event) = messages += e.toString + "\n"
    } 
    var reporter = new MockReporter
    var stopper = new org.scalatest.Stopper {}
    def plan: String = {
      s.run(None, reporter, new org.scalatest.Stopper {}, org.scalatest.Filter(), Map("plan" -> "true"), None, new org.scalatest.Tracker)
      reporter.messages
    }
    def expectations: String = "[1-9] expectation"
  }
  def help = {
    s.help
    s.messages.toList
  }
}