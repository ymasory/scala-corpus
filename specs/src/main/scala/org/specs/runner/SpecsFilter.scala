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
import java.util.regex.Pattern.compile
import java.util.regex.PatternSyntaxException
import org.specs._

trait SpecsFilter extends SpecsHolder {

  /** default regexp for filtering sus. */
  def susFilterPattern = ".*"

  /** default regexp for filtering examples. */
  def exampleFilterPattern = ".*"

  /** filtered specs to run. */
  lazy val filteredSpecs = filter(specs)

  /** pattern for the sus. */
  lazy val susFilter = compilePattern("sus", susPattern)

  /** pattern for the examples. */
  lazy val exampleFilter = compilePattern("example", examplePattern)

  private def compilePattern(description: String, pattern: String) = {
    try { compile(pattern) }
    catch {
      case e: PatternSyntaxException => throw new SpecsFilterPatternException(description, e)
      case other => throw other
    }
  }

  /**filter a list of specifications. */
  def filter(specifications: Seq[Specification]): List[Specification] = {
    specifications.flatMap(filter(_)).toList
  }

  /**
   * filter a specification.
   * @return None if the resulting specification has no subspecifications
   */
  def filter(specification: Specification): Option[Specification] = {
    specification.subSpecifications = specification.subSpecifications.flatMap(filter(_)).toList
    specification.systemsList = specification.systemsList.flatMap(filter(_)).toList
    if (specification.subSpecifications.isEmpty && specification.systems.isEmpty)
      None
    else
      Some(specification)
  }

  /**
   * filter a SUS.
   * @return None if the sus doesn't match the description
   */
  def filter(sus: Sus): Option[Sus] = {
    if (susFilter.matcher(sus.description).find) {
       sus.examplesFilter = { ex => filterExample(ex) }
       Some(sus)
    }
    else
      None
  }

  /**
   * filter one example.
   * @return None if the example does not match the expected regular expression
   */
  def filterExample(example: Example): Option[Example] = {
    if (exampleFilter.matcher(example.description).find)
      Some(example)
    else
      None
  }

  /**
   * @return either the system property named "sus" or the class attribute
   */
  def susPattern: String = {
    System.getProperty("sus") match {
      case null => susFilterPattern
      case something => something
    }
  }

  /**
   * @return either the system property named "example" or the class attribute
   */
  def examplePattern: String = {
    System.getProperty("example") match {
      case null => exampleFilterPattern
      case something => something
    }
  }
}
/** specific exception for pattern compilation errors */
case class SpecsFilterPatternException(description: String, cause: Exception) extends
    Exception("Wrong pattern for the " + description + " filter: " + cause.getMessage, cause)
