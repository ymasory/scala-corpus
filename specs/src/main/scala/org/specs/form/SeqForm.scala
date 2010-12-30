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
package org.specs.form
import scala.xml._
import org.specs.util.Plural._
import org.specs.execute.Status
/**
 * A SeqForm is a TableForm containing a sequence of LineForms
 * and using a sequence of values as the actual values
 * 
 * It is used in conjonction with a custom LineForm representing the values on a line: <code>
 * 
 * // @see EntityLineForm for LineForms representing a specific entity, possibly unset
 * case class CustomerLine(name: String, age: Int) extends EntityLineForm[Customer] {
 * // the prop method accepts a function here, taking the proper attribute on the "Entity"
 *   prop("Name", (_:Customer).getName)(name)
 *   prop("Age", (_:Customer).getAge)(age)
 * }
 * class Customers(actualCustomers: Seq[Customer]) extends SeqForm[T](actualCustomers)
 *  
 * // example usage
 * new Customers(listFromTheDatabase) {
 *   tr(CustomerLine("Eric", 36))
 *   tr(CustomerLine("Bob", 27))
 * }
 * </code>
 */
class SeqForm[T](title: Option[String], val seq: Seq[T]) extends TableForm(title) with SeqFormEnabled[T] {
  def this(s: String, seq: Seq[T]) = this(Some(s), seq)
  def this(seq: Seq[T]) = this(None, seq)
  def this() = this(None, List())
}
trait SeqFormEnabled[T] extends TableFormEnabled {
  val seq: Seq[T]
  /** list of declared lines which are expected but not received as actual */
  private var unmatchedLines = new scala.collection.mutable.ListBuffer[LineForm]
  /** number of already expected lines */
  private var expectedLinesNb = 0
  /** 
   * add a new line with a function taking an object (which may be None, if the set of actual values doesn't contain enough values), 
   * returning a LineForm
   * 
   * If this is the first line, a table header is added
   */
  def line(l : Option[T] => LineForm): LineForm = {
    var currentLine: LineForm = null
    if (expectedLinesNb >= seq.size) {
      currentLine = l(None)
      setHeader(currentLine)
      unmatchedLines.append(currentLine.comment)
      currentLine
    } else {
      currentLine = l(Some(seq(expectedLinesNb)))
      setHeader(currentLine)
      trs(currentLine.rows)
      form(currentLine)
    }
    expectedLinesNb = expectedLinesNb + 1
    currentLine
  }
  def tr(l: EntityLineForm[T]): LineForm = line { (actual: Option[T]) => 
    l.entityIs(actual)
  }
  override def setHeader[F <: LineForm](line: F): F = {
    if (rowsNb == 0) inNewRow(line.header)
    line
  }

  /**
   * upon execution a new row will be added to notify the user of unmatched lines
   */
  override def executeThis = {
    val i = unmatchedLines.size
    if (i > 0) { 
      th3("There ".bePlural(i) + " " + i + " unmatched line".plural(i), Status.Failure)
      unmatchedLines.foreach { (line: LineForm) => trs(line.rows) }
    }
    super.executeThis
  }
}
