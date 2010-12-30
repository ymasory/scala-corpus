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
import org.specs.specification._
import org.specs.util.ExtendedThrowable._
import org.specs.io._
import org.specs.util._
import org.specs._

/**
 * TeamCity string formatting utilities.
 */
object TeamCityUtils {
  class TeamCityString(s: String) {
    def quoteForTeamCity = "'" + escapeForTeamCity + "'"
    def escapeForTeamCity = s.replaceAll("['|\\]]", "|$0").replace("\r", "|r").replace("\n", "|n")
  }
  implicit def teamcityString(s: String) = new TeamCityString(s)
}

/**
 * The TeamCityOutput trait prints messages respecting the TeamCity format on the standard output.
 * @see http://www.jetbrains.net/confluence/display/TCD3/Build+Script+Interaction+with+TeamCity-testReporting
 * for more information.
 */
trait TeamCityOutput extends Output {
  /** create a message for the start of a test suite */
  def testSuiteStarted(name: String) = message("testSuiteStarted", "name" -> name)
  /** create a message for the end of a test suite */
  def testSuiteFinished(name: String) = message("testSuiteFinished", "name" -> name)
  /** create a message for the start of a test */
  def testStarted(name: String) = message("testStarted", "name" -> name)
  /** create a message for the end of a test */
  def testFinished(name: String) = message("testFinished", "name" -> name)
  /** create a message for an ignored test */
  def testIgnored(name: String, args: (String, String)*) = message("testIgnored", Seq("name" -> name) ++ args: _*)
  /** create a message for a failed test (failure or error) */
  def testFailed(name: String, args: (String, String)*) = message("testFailed", Seq("name" -> name) ++ args: _*)

  import TeamCityUtils._
  /** create a message with one argument */
  def message(messageType: String, arg: String) =
    println("##teamcity[" + messageType + " " + arg.quoteForTeamCity + "]")
    
  /** 
   * create a message with several arguments.
   * They should be outputed as ##teamcity[testFailed message='1 is not equal to 2']
   */
  def message(messageType: String, args: (String, String)*) =
    println("##teamcity[" + messageType + " " + args.map { case (n, v) => n + "=" + v.quoteForTeamCity }.mkString(" ") + "]")
}

/**
 * The TeamCityReporter is an output reporter tailored to output
 * messages using the TeamCity format.
 */
trait TeamCityReporter extends OutputReporter with TeamCityOutput {

  /** current specification being reported. */
  private val currentSpec = new scala.util.DynamicVariable[Specification](null)

  /** report a specification, on the TeamCity output. */
  override def reportSpec(spec: Specification, padding: String) = {
    testSuiteStarted(spec.name)
    currentSpec.withValue(spec) {
      super.report(spec.subSpecifications, padding)
      super.reportSystems(spec.systems, padding)
    }
    spec.executeAfterSpec
    testSuiteFinished(spec.name)
    this
  }
   
  /** report a Sus on the TeamCity output (). */
  override def reportSus(sus: Sus, padding: String) = { 
    testSuiteStarted(sus.description)
    for (example <- sus.examples) {
      reportExample(example, padding)
    }
    testSuiteFinished(sus.description)
  }
  /** 
   * print a Sus on the TeamCity output (this method is directly called
   * if there is only sus in the specification). 
   */
  override def printSus(sus: Sus, padding: String) = reportSus(sus, padding) 

  /** 
   * Report one example on the TeamCity output.
   * The subexample messages are aggregated as one TeamCity message.
   * 
   * In the TeamCity web interface, only the "details" field is shown. 
   * Exceptions stacktraces are mapped onto it.
   */
  override def reportExample(example: Examples, padding: String) = {
    val testName = currentSpec.value.name + "." + example.description
    testStarted(testName)
    
    if (!example.failureAndErrors.isEmpty) {
      def exampleMessages(e: Examples) = e.failureAndErrors.map(throwableToMessage _).mkString("; ")
      def subExampleMessages(e: Example) = e.failureAndErrors.map(e.description + ": " + throwableToMessage(_))
      val ms = if (example.examples.isEmpty)
                 exampleMessages(example)
               else
                 example.examples.flatMap(e => subExampleMessages(e)).mkString("; ")
      val ds = example.failureAndErrors.map(t => throwableToDetails(t))
      testFailed(testName, "message" -> ms, "details" -> ds.mkString("\n"))
    }
    if (!example.skipped.isEmpty) {
      val ms = example.skipped.map(t => throwableToMessage(t))
      testIgnored(testName, "message" -> ms.mkString("; "))
    }
    
    testFinished(testName)
  }
  
  /** @return the stacktrace of a Throwable as a String */
  private def throwableToDetails(t: Throwable) =
    t.printStackTraceToString
  
  /** @return the message (including its location) of a Throwable as a String */
  private def throwableToMessage(t: Throwable) = {
    (if (t.getMessage != null) t.getMessage else "no message") + 
    " (" + t.location + ")" 
  }
  override val timer = new SimpleTimer
}

/**
 * This runner provides a main method to take a list of specifications and
 * report them one by one.
 */
class TeamCityRunner(val specs: Specification*) extends TeamCityReporter
