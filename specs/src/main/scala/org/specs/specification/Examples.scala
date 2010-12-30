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

import org.specs.util.ExtendedString._
import org.specs.util._
import scala.xml._
import org.specs.matcher._
import scala.collection.mutable._
import org.specs.runner._
import org.specs.matcher.MatcherUtils._
import org.specs.SpecUtils._
import org.specs.specification._
import org.specs.util.ExtendedThrowable._
import scala.reflect.ClassManifest
import org.specs.execute._

/**
 * The <code>Examples</code> class specifies a list of examples of a system behaviour.<br/>
 * It either is an example containing other subexamples or a "terminal" example defining expectations on the
 * system behaviour.
 * It has:<ul>
 * <li>a description explaining what is being done
 * <li>an optional <code>ExampleLifeCycle</code> which defines behaviour before/after example and test</ul>
 * <p/>
 * 
 * Concrete instances of this class are the Example class and the Sus class. The main difference between those two classes 
 * is the methods they offer to create their body:<br/> 
 * <code>"this" should {...} for a Sus</code><br/> and 
 * <code>"this must behave like this" in {...}</code><br/>
 * 
 * One important thing to note is that subexamples are only created when the parent <code>Examples</code> is executed.
 * 
 * A Sus can also have a literate description when defined in a LiterateSpecification.
 */
abstract class Examples(var exampleDescription: ExampleDescription, val parentCycle: Option[LifeCycle]) extends
  ExampleContext with DefaultResults {
  parent = parentCycle
  /** this function gives a hint if this object has possibly subexamples */
  def hasSubExamples = true 
  /** example description as a string */
  def description = exampleDescription.toString
  /** @return the example description */
  override def toString = description
  /** @return a user message with failures and messages, spaced with a specific tab string (used in ConsoleReport) */
  def pretty(tab: String) = tab + description + failures.foldLeft("") {_ + addSpace(tab) + _.message} +
                                                errors.foldLeft("") {_ + addSpace(tab) + _.getMessage}

  /** 
   * execute this example, by delegating the execution to the execution instance
   * If the execution of this example comes from a cloned example (see SpecificationExecutor)
   * Then all the execution results need to be copied back to this example.
   */
  def executeThis = {
    execution.map(_.execute)
    execution.map { e => if (!(e.example eq this))
      this.copyExecutionResults(e.example) 
    }
  }

  /** execute this example to  be able to get all subexamples if any. */
  def executeExamples = {
    if (!executed) 
      parent.map(_.executeExample(this))
  }
  /**
   * @return all the examples from this example, this is either this if there are no subexamples or the recursive list of
   * all examples taken on each subexample
   */
  override def allExamples: List[Examples] = {
    if (examples.isEmpty)
      List(this)
    else
      examples.flatMap(_.allExamples).toList
  }
  /** @return the example for a given Tree path */
  def getExample(path: TreePath): Option[Examples] = {
    path match {
      case TreePath(List()) => 
        return Some(this)
      case TreePath(i :: rest) if !this.examples.isEmpty => 
        this.examples(i).getExample(TreePath(rest))
      case _ => 
        None
    }
  }
  /** 
   * set the main block to execute when "execute" will be called
   * In this block we make sure that this example will be the container for any subexample created during
   * the execution of the code.<br/>
   * 
   * If the lifecycle specifies that the specification is sequential, then the example is executed right away.
   */
  def specifyExample(a: =>Any): this.type = {
    execution = Some(new ExampleExecution(this, (ex) => withCurrent(ex) { a }))
    if (isSequential)
      executeExamples
    this
  }
  /** alias for specifyExample */
  def specifies(a: =>Any): this.type = specifyExample(a)
  /** increment the number of expectations in this example */
  def addExpectation: Examples = { thisExpectationsNumber += 1; this }
  /** create a new example with a description and add it to this. */
  def createExample(desc: String): Example = {
    val ex = new Example(desc, this)
    addExample(ex)
    ex
  }
  /**
   * set the execution context for a cloned example: tags, filter, beforeFirst failure
   */
  override private[specification] def prepareExecutionContextFrom(other: Examples) = {
    super.prepareExecutionContextFrom(other)
    other.parentCycle match {
      case Some(p) => this.beforeSystemFailure = p.beforeSystemFailure
      case None => ()
    }
  }
}
