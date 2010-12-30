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
import org.specs.log.ConsoleLog
import org.specs.collection.JavaCollectionsConversion
import _root_.org.junit.runner._
import org.specs.specification._
import org.specs.Specification

/**
 * The Runner class is an abstract class referencing one or several specifications to run. It should be extended
 * with at least one runner trait which will use the <code>specifications</code>.
 * Usage:<code>
 * class mySpecRunner extends Runner(mySpec) with JUnit with Console with ScalaTest
 * or
 * class mySpecRunner extends Runner(mySpec) with JUnit with Xml with ScalaTest
 * which will also output the results in an xml file
 * <code>
 *
 * Then mySpecRunner class can be executed in many ways:<ul>
 * <li>scala -cp ... -e "new mySpecRunner.reportSpecs"
 * <li>java -cp ... org.scalatest.Runner -g -s mySpecRunner
 * <li>java -cp ... org.junit.runner.JUnitCore mySpecRunner
 * </ul>
 *
 * It is annotated with a JUnit annotation because JUnit requires that annotation should be placed on the class which will be executed.
 * In the example above, Runner(mySpec) is the only class; JUnit, Console and ScalaTest are all traits.
 */
@RunWith(classOf[JUnitSuiteRunner])
class Runner(val specifications: Seq[Specification], val reporters: Seq[Reporter]) extends Reporter {

  /** alternate constructor with a specs holder (possibly a SpecsFinder object). */
  def this(specsHolder: SpecsHolder, reps: Seq[Reporter]) = this(specsHolder.specs, reps)
  def this(specsHolder: SpecsHolder*) = this(specsHolder.flatMap(_.specs), List(new ConsoleRunner))
  val specs = specifications
  override def report(specs: Seq[Specification]) = {
    super.report(specs)
    reporters foreach { reporter =>
      reporter.args = reporter.args ++ this.args
      reporter.report(specs)
    }
    this
  }
}
