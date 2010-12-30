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
import org.specs.literate._
import org.specs.util._
import scala.collection.mutable._
import org.specs.io.mock.MockOutput
import org.specs.Sugar._
import org.specs.matcher.MatcherUtils._
import org.specs.util.ExtendedString._
import org.specs.execute._

class consoleReporterSpec extends SpecificationWithJUnit {
  include(new reporterSpecification, new consoleTraitSpecification)
}
class reporterSpecification extends TestSpecs {
  "A console reporter" should {
    "report the time for each system and add times for the total" in {
      val specWithTwoSystems = new SpecWithTwoSystems()
      specWithTwoSystems.messages
      val susTime1 :: susTime2 :: total :: Nil = specWithTwoSystems.elapsedTimes
      (susTime1 + susTime2) must beCloseTo(total, 50) // to account for rounding errors
    }
    "report the name of the specification: 'A specification should'" in {
      specWithOneExample(that.isOk) must containMatch("A specification should")
    }
    "report the specification examples: '-have example 1 ok'" in {
      specWithOneExample(that.isOk) must containMatch("have example 1 ok")
    }
    "display '0 failure' if there is no expectation" in {
      specWithOneExample(that.isOk) must containMatch("0 failure")
    }
    "display '1 failure' if one example isKo" in {
      specWithOneExample(that.isKo) must containMatch("1 failure")
    }
    "indicate the line and class where the failure occurred" in {
      specWithOneExample(that.isKo) must containMatch("(consoleReporterSpec.scala:\\d)")
    }
    "display the first failure of an example having several failures" in {
      specWithOneExample(that.isKo, that.isKo) must containMatch("first failure")
      specWithOneExample(that.isKo, that.isKo) must notContainMatch("second failure")
    }
    "display the failures of subexamples" in {
      val specWithTwoSubExamples = new SpecWithSubexamples
      specWithTwoSubExamples.reportSpecs
      specWithTwoSubExamples.messages must containMatchOnlyOnce("1.1")
    }
    "display '1 error' if one example throws an exception" in {
      specWithOneExample(that.throwsAnException) must containMatch("1 error")
    }
    "display the exception type if one example throws an exception" in {
      specWithOneExample(that.throwsAnException) must containMatch("java.lang.Exception")
    }
    "display '1 skipped' if one example is skipped" in {
      specWithOneExample(that.isSkipped) must containMatch("1 skipped")
    }
    "report a pluralized message if there are several examples failing" in {
      specWithTwoExamples(that.isKo) must containMatch("2 examples")
      specWithTwoExamples(that.isKo) must containMatch("2 failures")
    }
    "report the number of expectations: '2 expectations'" in {
      specWithOneExample(that.isOk) must containMatch("1 expectation")
      specWithTwoExamples(that.isKo) must containMatch("2 expectations")
    }
    "display the failure message next to the corresponding example" in {
      specWithTwoExamples(that.isKo, that.isOk) verifies(messages =>
            messages.indexWhere(matches("first failure")) ==
            messages.indexWhere(matches("example 2.1 ok")) + 1)
    }
    "display nested examples in the right order" in {
      specWithOneExampleAndTwoNestedExamples verifies(messages =>
            messages.findIndexOf(matches("ex 1.1")) ==
            messages.findIndexOf(matches("ex 1.2")) - 1)
    }
    "report the elapsed time" in {
      specWithOneExample(that.isOk) mustContainMatch "Finished in"
    }
    "report failures created with the 'fail' method" in {
      specWithOneExample(that.isKoWithTheFailMethod) mustContainMatch "1 failure"
    }
    "report skipped examples created with the 'skip' method with a small circle" in {
      specWithOneExample(that.isSkipped) mustContainMatch "o "
    }
    "report skipped examples created with the 'orSkipExample' on a faulty matcher with a small circle" in {
      specWithOneExample(that.isSkippedBecauseOfAFaultyMatcher) mustContainMatch "o "
    }
    "report the literal description of a sus if it is set"  in {
      specWithOneExample(that.isOk).toList must not containMatch("(  ,)")
    }
    "not report the literal description of a sus if it is set"  in {
      new SpecWithLiterateDescription(that.isOk).run mustContainMatch "Some text with embedded expectations"
    }
    "report the reason for a skipped example" in {
      specWithOneExample(that.isSkipped) mustContainMatch "irrelevant"
    }
    "indicate the line and class where the skipping occurred" in {
      specWithOneExample(that.isSkipped) must containMatch("(consoleReporterSpec.scala:\\d)")
    }
    "not print out the description of an anonymous system" in {
      val spec = new SpecWithAnAnonymousSystem(that.isOk)
      spec.run mustNot containMatch("specifies")
    }
    "not print stack trace if setNoStackTrace is called" in {
      val spec = new SpecWithOneExample(that.throwsAnException)
      spec.setNoStacktrace()
      spec.run mustNot containMatch("org.specs.runner.SpecWithOneExample\\$")
    }
    "not print out an empty sus" in {
      new SpecWithAnEmptySus().run.toList must not containMatch("An empty system")
      new SpecWithAnEmptySus().run.toList must not containMatch("Total for SUS")
    }
    "print only the high-level examples of an anonymous sus" in {
	  new SpecWithAnAnonymousSystem(that.isOk).run.toList must not containMatch("specifies")
	}
    "execute the actions after a specification even if the specification is sequential" in {
	  new SequentialSpecWithAfterSpecification().run.toList must containInOrder("  + ex2", "afterSpec")
	}
  }
}
class consoleTraitSpecification extends TestSpecs {
  val specRunner = new ConsoleRunner(new specWithTags) with MockOutput
  val specTwoSystemsRunner = new ConsoleRunner(specTwoSystems) with MockOutput
  "A console trait" should {
    "setNoStackTrace on the ConsoleReporter when passed the -ns or --nostacktrace argument" in {
      val testSpecRunner = new SpecWithOneExample(that.throwsAnException) with MockOutput
      testSpecRunner.args ++= Array("-ns")
      testSpecRunner.reportSpecs
      testSpecRunner.messages mustNot containMatch("org.specs.runner.SpecWithOneExample\\$")
    }
    "not display the sus at all if all examples are ok with the -xonly flag" in {
      runWith("-acc", "in", "-xonly") must notContainMatch("this sus")
    }
    "work with several tags separated by a comma" in {
      runWith("-acc", "in,out") must containMatch("\\+ included") 
      runWith("-acc", "in,out") must containMatch("\\+ excluded")
    }
    "print a warning message if a accept/reject argument is not followed by tags" in {
      runWith("-acc") must containMatch("\\[WARNING\\] accept/reject tags omitted")
    }
  }
  "A console trait with tagged specifications" should { 
    "accept a --reject argument to only exclude examples having some tags in the specification" in {
      runWith("--reject", "out", "-ns") must (containMatch("\\+ included") and containMatch("o excluded"))
    }
    "accept a -rej argument to only exclude examples having some tags in the specification" in {
      runWith("-rej", "out", "-ns") must (containMatch("\\+ included") and containMatch("o excluded"))
    }
    "accept a --accept argument to only include examples having some tags in the specification" in {
      runWith("--accept", "in", "-ns") must (containMatch("\\+ included") and containMatch("o excluded"))
    }
    "accept a -acc argument to only exclude examples having some tags in the specification" in {
      runWith("-acc", "in", "-ns") must (containMatch("\\+ included") and containMatch("o excluded"))
    }
  }
  "A console trait" should {
    "not display the statistics with the -finalstats or --finalstatistics flag" in {
      run2SystemsWith("-finalstats") must notContainMatch("for SUS")
    }
    "not display the statistics with the -nostats or --nostatistics flag" in {
      runWith("-nostats") must notContainMatch("Total time")
    }
    def asString(s: String) = s.replace("\\", "\\\\").replace("[", "\\[")
    "report statuses with ANSI color codes when passed the -c or --color flag" in {
      runWith("--color") must containMatch(asString(AnsiColors.green))
    }
    "report a success in green when passed the -c or --color flag" in {
      runWith("-c") must containMatch(asString(AnsiColors.green))
    }
    "report a failure in red when passed the -c or --color flag" in {
      runWith("-c") must containMatch(asString(AnsiColors.red))
    }
    "report an error in red when passed the -c or --color flag" in {
      runWith("-c") must containMatch(asString(AnsiColors.red))
    }
    "report a skipped example in yellow when passed the -c or --color flag" in {
      runWith("-c") must containMatch(asString(AnsiColors.yellow))
    }
    "print a help message with the options description if passed the -h or --help flag" in {
      mainWith("--help") must containMatch("--help")
    }
    "not execute the specification when passed the -h or --help flag" in {
      mainWith("--help") must notContainMatch("this sus")
    }
  }
  def runWith(args: String*): List[String] = {
    specRunner.args = args.toArray
    specRunner.clearMessages
    specRunner.reportSpecs
    specRunner.messages.toList
  }
  def run2SystemsWith(args: String*): List[String] = {
    specTwoSystemsRunner.args = args.toArray
    specTwoSystemsRunner.reportSpecs
    specTwoSystemsRunner.messages.toList
  }
  def mainWith(args: String*): List[String] = {
    specRunner.clearMessages
    specRunner.main(args.toArray)
    specRunner.messages.toList
  }
}
class TestSpecs extends org.specs.Specification {
  def specWithOneExample(expectations: (that.Value)*) = new SpecWithOneExample(expectations.toList).run
  def specWithTwoExamples(expectations: (that.Value)*) = new SpecWithTwoExamples(expectations.toList).run
  def specWithOneExampleAndTwoNestedExamples = new SpecWithOneExampleAndTwoNestedExamples().run 
  def specTwoSystems = new Specification {
    "this is system one" should { "do nothing" in { 1 must_== 1 } }
    "this is system two" should { "do nothing" in { 1 must_== 1 } }
  }
}
class specWithTags extends Specification {
  "this sus" should {
    ("excluded" in { 1 must_== 1 }).tag("out")
    ("included" in { 1 must_== 1 }).tag("in")
    "failed" in { 1 must_== 0 }
    "error" in { 1 / 0 }
    "skipped" in { skip("skipped"); 1 }
  } tag "in"
}
abstract class TestSpecification extends org.specs.Specification with Expectations with MockOutput {
  override val specs = List(this)
}
trait Expectations  { this: org.specs.Specification =>
  val success = () => true mustBe true
  val isSkipped = () => skip("irrelevant")
  val isSkippedBecauseOfAFaultyMatcher = () => 1 must be(0).orSkipExample
  val failure1 = () => "ok" mustBe "first failure"
  val failure2 = () => "ok" mustBe "second failure"
  val failMethod = () => fail("failure with the fail method")
  val exception= () => throw new Exception("new Error")
  val exceptionWithACause = () => {
    throw new Exception("new Error", new NullPointerException)
  }
  def expectations(behaviours: List[that.Value]) = behaviours map {
                                    case that.isOk => success
                                    case that.isSkipped => isSkipped
                                    case that.isSkippedBecauseOfAFaultyMatcher => isSkippedBecauseOfAFaultyMatcher
                                    case that.isKo => failure1
                                    case that.isKoTwice => () => {failure1(); failure2()}
                                    case that.isKoWithTheFailMethod => failMethod
                                    case that.throwsAnException => exception
                                    case that.throwsAnExceptionWithACause => exceptionWithACause 
  }
}

class SpecWithOneExample(behaviours: List[(that.Value)]) extends TestSpecification {
  def run = {
    "A specification" should {
       "have example 1 ok" in {
        expectations(behaviours) foreach {_.apply}
      }
    }
    reportSpecs
    messages
  }
}
class SpecWithTwoExamples(behaviours: List[(that.Value)]) extends TestSpecification {
  def run = {
    "A specification" should {
      "have example 2.1 ok".specifies { expectations(behaviours).head.apply; () }
      "have example 2.2 ok".specifies { expectations(behaviours).last.apply; () }
    }
    reportSpecs
    messages
  }
}
class SpecWithAnAnonymousSystem(behaviours: List[(that.Value)]) extends TestSpecification {
  def run = {
    "have example 1 ok" in {
      expectations(behaviours) foreach {_.apply}
    }
    reportSpecs
    messages
  }
}
class SequentialSpecWithAfterSpecification extends TestSpecification {
  setSequential()
  shareVariables()
  doAfterSpec { println("afterSpec") }
  def run = {
    "it" should {
      "ex1" in { 1 must_== 1 }
      "ex2" in { 1 must_== 1 }
    }
    reportSpecs
    messages
  }
}
class SpecWithAnEmptySus extends TestSpecification {
  def run = {
    "An empty system" should {
    }
    reportSpecs
    messages
  }
}
class SpecWithSubexamples extends TestSpecification {
  "A specification" should {
      "have example 1 ok" in { "1.1" in { 1 must_== 1 } }
      "have example 2 ok" in { "2.1" in { 1 must_== 1 } }
  }
}
class SpecWithTwoSystems extends TestSpecification {
    "A specification" should {
      "have example 2.1 ok" in { Thread.sleep(10) }
      "have example 2.2 ok" in { Thread.sleep(10) }
    }
    "A specification" should {
      "have example 2.1 ok" in { Thread.sleep(10) }
      "have example 2.2 ok" in { Thread.sleep(10) }
    }
    def elapsedTimes = {reportSpecs; messages.flatMap(_.groups("Finished in .* (\\d+) ms")).filter(!_.isEmpty).toList.map(_.toInt)}
}
class SpecWithLiterateDescription(behaviours: List[(that.Value)]) extends LiterateSpecification with Expectations 
with MockOutput with Textile {
  def run = {
    "The specification" is <p>
      Some text with {"embedded expectations" in {expectations(behaviours) foreach {_.apply}}}
    </p>
    reportSpecs
    messages
  }
}
class SpecWithOneExampleAndTwoNestedExamples extends Specification with MockOutput {
  "this system" should {
    "ex 1" in {
      "ex 1.1" in { 1.isExpectation }
      "ex 1.2" in { 1.isExpectation }
    }
  }
  def run = {
    reportSpecs
    messages
  }
}
object that extends Enumeration {
  val isKo, isOk, isKoTwice, isKoWithTheFailMethod,
      throwsAnException, throwsAnExceptionWithACause, isSkipped, isSkippedBecauseOfAFaultyMatcher = Value
}
