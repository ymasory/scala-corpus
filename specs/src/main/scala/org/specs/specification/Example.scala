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
 * The Example class represents a block of code with expectations.<br/>
 * 
 * It has a description and an optional context.
 * 
 * Usage: <code>"this is an example" in { // code containing expectations }</code> or<br>
 * <code>"this is an example" >> { // code containing expectations }</code><br>
 * ">>" can be used instead of "in" if that word makes no sense in the specification
 * <p>
 * An example can also contain subexamples which are executed will evaluating the <code>in</code> method.
 * <p>
 * When expectations have been evaluated inside an example they register their failures and errors for later reporting
 */
class Example(var exampleDesc: ExampleDescription, private var p: Option[ExampleContext]) extends Examples(exampleDesc, p) {
  /** constructor with a simple string */
  def this(desc: String, parent: ExampleContext) = this(ExampleDescription(desc), Some(parent))
  /** constructor with a simple string */
  def this(desc: String) = this(ExampleDescription(desc), None)
  /** is set to true if this example contains subexamples, which we know by the type of the object passed to the in method */
  private[specification] var hasSomeSubExamples = false
  /** @return true if this example contains subexamples */
  override def hasSubExamples = hasSomeSubExamples
  /**
   * create a new Example object and store as an ExampleExecution object the expectations to be executed.
   * This <code>expectations</code> parameter is a block of code which may contain expectations with matchers.
   * Upon execution, errors and failures will be attached to the current example
   * by calling the <code>addFailure</code> and <code>addError</code> methods
   * Execution will be triggered when requesting status information on that example: failures, errors, expectations number, subexamples
   * @return a new <code>Example</code>
   */
  /** this version of in allows to declare examples inside examples */
  def in[T](expectations: =>T)(implicit m: scala.reflect.ClassManifest[T]): this.type = {
    specifyExample(expectations)
    if (m.erasure == this.getClass)
      hasSomeSubExamples = true
    this
  }

  /** alias for the <code>in</code> method to create subexamples */
  def >>[T](expectations: =>T)(implicit m: scala.reflect.ClassManifest[T]) = in(expectations)
  /**
   * when an example has been executed in another specification to guarantee its isolation
   * copy its state
   */
  override def copyExecutionResults(other: Examples) = {
    this.hasSomeSubExamples = other.hasSubExamples
    super.copyExecutionResults(other)
  }
}

/**
 * Description of the example. It can possibly be a piece of html code to display in a literate specification
 */
case class ExampleDescription(desc: String, toXhtml: Node) {
  override def toString = desc
  def format: String = toXhtml.toString
}
/**
 * Case object to build Example descriptions with just a string.
 */
object ExampleDescription {
  def apply(desc: String): ExampleDescription = ExampleDescription(desc, <ex>{desc}</ex>)
}
