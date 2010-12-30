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
import org.specs.util._
import org.specs.util.ExtendedString._
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
 * The <code>Sus</code> class represents a system under specification<br>
 * It has:<ul>
 * <li>a description declaring what kind of system it is
 * <li>an <code>ExampleLifeCycle</code> which defines behaviour before/after example and test</ul>
 * It is also an <code>ExampleLifeCycle</code> so it can refine the passed cycle
 * <p>
 * In specifications, a Sus "should" or "can" provide some functionalities which are defined in <code>Examples</code><br>
 * A Sus is "executed" during its construction and failures and errors are collected from its examples
 */
case class Sus(desc: String, specification: BaseSpecification) extends Examples(ExampleDescription(desc), Some(specification)) {

  /** constructor for an anonymous sus */                                        
  def this(parent: BaseSpecification) = this("specifies", parent)
  /** default verb used to define the behaviour of the sus */
  var verb = ""
  /** 
   * instead of using several examples, a whole text with embedded expectations can be used to
   * specify the Sus
   */
  var literateDescription: Option[LiterateDescription] = None
  /** header for the full sus description: description + " " + verb */
  def header = description + " " + verb
  /** @return true if the description is the generic one for anonymous systems */
  def isAnonymous = desc == "specifies"
  /** @return a description of this sus with all its examples (used for the ConsoleReporter) */
  override def pretty(tab: String) = tab + header + " " + examples.foldLeft("")(_ + _.pretty(addSpace(tab)))
  /** @return an xhtml literate description of the sus */
  def literateDesc: NodeSeq = literateDescription.map(_.toXhtml).getOrElse(NodeSeq.Empty)
    /** @return a String literate description of the sus */
  def literateDescText: String = literateDesc(0).text
  /** default way of defining the behaviour of a sus */
  def should(ex: =>Examples) = {
    verb = "should"
    specifyExample(ex)
    this
  }
  /** defining the behaviour of a sus, for examples which should be prefixed by the same word */
  def should(ex: PrefixedExamples) = {
    verb = ex.prepend("should")
    ex.example.map(e => specifyExample(e()))
    this
  }
  /** alternately there may be no example given yet */
  def should(noExampleGiven: =>Unit): Unit = { 
    verb = "should"
    specifyExample(noExampleGiven)
  }
  /** Alias method to describe more advanced or optional behaviour. This will change the verb used to report the sus behavior */
  def can(ex: =>Examples) = { 
    verb = "can"
    specifyExample(ex)
    this
  }
  /** Alias method to describe the sus behavior, for examples which should be prefixed by the same word */
  def can(ex: PrefixedExamples) = { 
    verb = ex.prepend("can")
    ex.example.map(e => specifyExample(e()))
    this
  }
  /** alternately there may be no example given yet */
  def can(noExampleGiven: =>Unit): Unit = { 
    verb = "can"
    specifyExample(noExampleGiven)
  }
}

/** support class representing the formatted literate description of a SUS */
case class LiterateDescription(desc: Node) {
  def toXhtml: NodeSeq = desc
}
