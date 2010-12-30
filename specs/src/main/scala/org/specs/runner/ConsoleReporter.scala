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
import org.specs.io._
import java.util.Calendar
import org.specs.specification._
import org.specs.util._
import org.specs._
import org.specs.specification._
import org.specs.util.ExtendedThrowable._
import org.specs.execute._
import org.specs.util.Plural._
/**
 * This trait reports the result of a specification on a simple <code>Output</code>
 * which must support <code>print</code>-like methods
 */
trait OutputReporter extends Reporter with Output {

  /** colors the text in red if colors are enabled   */
  def failureColored(text: String) =
    if (colorize()) AnsiColors.red + text + AnsiColors.reset
    else text

  /** colors the text in green if colors are enabled   */
  def successColored(text: String) =
    if (colorize()) AnsiColors.green + text + AnsiColors.reset
    else text

  /** colors the text in yellow if colors are enabled   */
  def skipColored(text: String) =
    if (colorize()) AnsiColors.yellow + text + AnsiColors.reset
    else text

  /** colors the text in blue if colors are enabled   */
  def infoColored(text: String) =
    if (colorize()) AnsiColors.blue + text + AnsiColors.reset
    else text


  /** the timer is used to display execution times */
  val timer: Timer

  /**
   * override the parent method for arguments setting and
   * call the local report method with no padding to being with.
   */
  override def report(specs: Seq[Specification]): this.type = {
    super.report(specs)
    report(specs, "")
  }

  /**
   * reports a list of specifications with a given space separator to display before the results.<br>
   * This method may be called recursively by the <code>reportSpec</code> method if a specification
   * has subSpecifications, hence the <code>padding</code> will be incremented
   */
  def report(specs: Seq[Specification], padding: String): this.type = {
    specs foreach (reportSpec(_, padding))
    this
  }

  /**
   * reports a specification with a given space separator to display before the results.<br>
   * This method may be called recursively by the <code>reportSpec</code> method if a specification
   * has subSpecifications, hence the <code>padding</code> will be incremented
   */
  def reportSpec(spec: Specification, padding: String): this.type = {
    timer.start
    println(padding + "Specification \"" + spec.name + "\"")
    report(spec.subSpecifications, padding + "  ")
    reportSystems(spec.systems, padding + "  ")
    spec.executeAfterSpec
    timer.stop

    // if we want final statistics only, we check the padding to know if we're
    // reporting the first specification. An empty padding means this is the first spec.
    val isFirstSpecification = padding.isEmpty
    if (statistics() && (!finalStatisticsOnly() ||
                         finalStatisticsOnly() && isFirstSpecification))  {
      println(padding + "Total for specification \"" + spec.name + "\":")
      printStats(stats(spec), padding)
    }
    this
  }

  /** utility implicit definition to be able to add tuples */
  implicit def toAddableTuple(t1: Tuple5[Int, Int, Int, Int, Int]) = new AddableTuple(t1)
  class AddableTuple(t1: Tuple5[Int, Int, Int, Int, Int]) {  def +(t2: Tuple5[Int, Int, Int, Int, Int]) = (t1._1 + t2._1, t1._2 + t2._2, t1._3 + t2._3, t1._4 + t2._4, t1._5 + t2._5) }

  /**
   * @return the number of examples, expectations, failures and errors for a specification
   * by collecting those numbers on sub-specifications and systems
   */
  def stats(spec: Specification): (Int, Int, Int, Int, Int) = {
    spec.systems.foldLeft((0, 0, 0, 0, 0))(_ + stats(_)) +
    spec.subSpecifications.foldLeft((0, 0, 0, 0, 0))(_ + stats(_))
  }

  /**
   * @return the number of examples, expectations, failures and errors for a sus
   * by collecting those numbers on examples
   */
  def stats(sus: Sus): (Int, Int, Int, Int, Int)  = {
    sus.examples.foldLeft((0, 0, 0, 0, 0))(_ + stats(_))
  }

  /**
   * @return the number of examples, expectations, failures and errors for an example
   * by collecting those numbers on this example and on sub-examples
   */
  def stats(example: Example): (Int, Int, Int, Int, Int) = {
    if (!planOnly()) {
     (if (example.examples.isEmpty) 1 else 0, example.ownExpectationsNb, example.ownFailures.size, example.ownErrors.size, example.ownSkipped.size) +
     example.examples.foldLeft((0, 0, 0, 0, 0))(_ + stats(_))
    } else
     (1, 0, 0, 0, 0)
  }

  /**
   * reports the sus results. If there are more than one, then report stats for each
   * else just print the specification of the sus, the parent specification will display the total
   * for that sus
   */
  def reportSystems(systems: Iterable[Sus], padding: String) = {
    def displaySus(s: Sus) = if (systems.toList.size > 1) reportSus(s, padding) else printSus(s, padding)
    systems foreach { s =>
      if (canReport(s) && (!s.examples.isEmpty || s.hasOwnFailureOrErrors)) {
        if (s.isAnonymous)
		  reportExamples(s.examples, padding)
		else
		  displaySus(s)
      }
    }
  }

  /**
   * reports one sus results: print the sus specifications, then the statistics
   */
  def reportSus(sus: Sus, padding: String) = {
    printSus(sus, padding);
    if (statistics() && !finalStatisticsOnly() && !sus.examples.isEmpty) 
      printStats(sus, padding)
  }

  /**
   * prints one sus specification
   */
  def printSus(sus: Sus, padding: String) = {
    var susDescription = if (sus.isAnonymous) "" else sus.header

    if (!sus.literateDesc.isEmpty) 
      println(padding + sus.literateDescText)
    else
      println(padding + susDescription)
    timer.start
    if (!planOnly() && sus.hasOwnFailureOrErrors)
      reportExample(sus, padding)
    reportExamples(sus.examples, padding)
    timer.stop
    println("")
  }
  /**
   * prints the statistics for a sus
   */
  def printStats(sus: Sus, padding: String): Unit = {
    println(padding + "Total for SUS \"" + sus.description + "\":")
    printStats(stats(sus), padding)
  }

  /**
   * prints the statistics for a specification
   */
  def printStats(stat: (Int, Int, Int, Int, Int), padding: String) = {
    val (examplesNb, expectationsNb,  failuresNb, errorsNb, skippedNb) = stat
    def failureColoredIf(text: String, cond: Boolean) =
      if (cond) failureColored(text)
      else text
    println(padding + "Finished in " + timer.time)
    println(padding +
            examplesNb + " example".plural(examplesNb) +
            (if (skippedNb > 0) " (" + skippedNb + " skipped)" else "") + ", " +
            expectationsNb + " expectation".plural(expectationsNb) + ", " +
            failureColoredIf(failuresNb + " failure".plural(failuresNb), failuresNb > 0) + ", " +
            failureColoredIf(errorsNb + " error".plural(errorsNb), errorsNb > 0)
            )
    println("")
  }

  /**
   * reports a list of examples and indent subexamples if there are some
   */
  def reportExamples(examples: Iterable[Example], padding: String): Unit = {
    for (example <- examples) {
      reportExample(example, padding)
      if (!planOnly())
        reportExamples(example.examples, padding + "  ")
    }
  }

  /**
   * reports one example: + if it succeeds, x if it fails, its description, its failures or errors
   */
  def reportExample(example: Examples, padding: String) = {
    def status(example: Examples) = {
      if (planOnly())
        infoColored("-")
      else if (example.hasFailureOrErrors)
        failureColored("x")
      else if (example.skipped.size > 0)
        skipColored("o")
      else
        successColored("+")
    }

    if (planOnly() || canReport(example))
      println(padding + status(example) + " " + example.description)

    // if the failure, skip or the error message has linefeeds they must be padded too
    def parens(f: Throwable) = " (" + f.location + ")"

    // only print out the example messages if there are no subexamples.
    if (!planOnly() && example.examples.isEmpty) {
      def errorType(t: Throwable) = t match {
        case s: SkippedException => ""
        case f: FailureException => ""
        case e => e.getClass.getName + ": "
      }
      example.skipped.toList ::: example.failures.toList ::: example.errors.toList foreach { f: Throwable =>
	    if (f.getMessage != null)
	      println(padding + "  " + errorType(f) + f.getMessage.replaceAll("\n", "\n" + padding + "  ") + parens(f))
	    else
	      println(padding + errorType(f) + parens(f))
	  }
	  if (stacktrace() && example.errors.size > 0) example.errors foreach { printStackTrace(_) }
    }
  }
  /** @return true if the results should be printed
   */
  private def canReport(hasResults: HasResults) = {
    !failedAndErrorsOnly() || failedAndErrorsOnly() && hasResults.hasFailureOrErrors
  }
}

/**
 * Implementation of the <code>OutputReporter</code> with a <code>ConsoleOutput</code>
 * and a <code>SimpleTimer</code>
 */
trait Console extends OutputReporter with ConsoleOutput {
  /** this timer uses java Calendar to compute hours, minutes, seconds and milliseconds */
  val timer = new org.specs.util.SimpleTimer
}

/**
 * This class implements the <code>Console</code> trait and can be initialized with specifications directly<br>
 * Usage: <code>object mySpecRunner extends ConsoleRunner(mySpec1, mySpec2)</code>
 */
class ConsoleRunner(val specifications: Specification*) extends Console {
  val specs = specifications
  def this() = this(Nil:_*)
  def ConsoleRunner(specs: List[Specification]) = new ConsoleRunner(specs :_*)
}


/**
 * This object provides AnsiColors codes for the OutputReporter
 * @see http://en.wikipedia.org/wiki/ANSI_escape_code
 */
object AnsiColors {
  val black   = "\033[30m"
  val red     = "\033[31m"
  val green   = "\033[32m"
  val yellow  = "\033[33m"
  val blue    = "\033[34m"
  val magenta = "\033[35m"
  val cyan    = "\033[36m"
  val white   = "\033[37m"

  val reset   = "\033[0m"
}
