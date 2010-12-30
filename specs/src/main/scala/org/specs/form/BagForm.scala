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
import scala.collection.mutable.ListBuffer
import scala.xml._
import org.specs.util.Plural._
import org.specs.util.Matching._
import org.specs.execute.Status
import org.specs.collection.ExtendedList._
import org.specs.execute.FailureException
/**
 * A BagForm is a TableForm containing a Bag of LineForms
 * and using a Bag of values as the actual values.
 * 
 * It works like a SeqForm but the order of the rows is not relevant
 * @see SetForm
 */
class BagForm[T](title: Option[String], val bag: Seq[T]) extends TableForm(title) with BagFormEnabled[T] {
  def this(title: String) = this(Some(title), List[T]())
  def this(bag: Seq[T]) = this(None, bag)
  def this() = this(None, List[T]())
}
trait BagFormEnabled[T] extends TableFormEnabled {
  val bag: Seq[T]
  /** true if all actual rows are to be expected */
  private var complete = true
  /** set the complete flag */
  def setComplete(c: Boolean): this.type = { complete = c; this }
  /** this won't create a failure if there are unmatched actual rows */
  def isIncomplete: this.type = setComplete(false)
  /** list of declared lines which are expected but not received as actual */
  private val expectedEntities = new ListBuffer[EntityLineForm[T]]
  def expectedLines = expectedEntities.toList
  private var unsetHeader = true

  override def tr[F <: Form](l: F): F = {
    l match {
      case entityLine: EntityLineForm[_] => {
        expectedEntities.append(entityLine.asInstanceOf[EntityLineForm[T]])
        properties.append(entityLine.asInstanceOf[EntityLineForm[T]])
      }
      case _ => super[TableFormEnabled].tr(l)
    }
    l
  }
  override def setHeader[F <: LineForm](line: F): F = {
    if (rowsNb == 0) inNewRow(line.header)
    line
  }

  /**
   * upon execution a new row will be added to notify the user of unmatched lines
   */
  override def executeThis = {
    matchedLines.foreach(_.genericFormatterIs(genericFormatter))
    addLines(matchedLines)
    val i = unmatchedExpectedLines.size
    if (i > 0) { 
      th3("There ".bePlural(i) + " " + i + " unmatched expected line".plural(i), Status.Failure)
      unmatchedExpectedLines.foreach { (line: EntityLineForm[T]) => {
          trs(line.testWith(None).reset().rows) 
        }
      }
    }
    val j = unmatchedActual.size
    if (j > 0 && complete) {
      val message = "There ".bePlural(j) + " " + j + " unmatched actual line".plural(j)
      addFailure(new FailureException(message))
      th3(message, Status.Failure)
      unmatchedActual.foreach { (actual: T) => th3(actual.toString) }
      throw new FailureException(message)
    }
    this
  }
  val edgeFunction = (t: (EntityLineForm[T], T)) => t
  val edgeWeight = (l: (EntityLineForm[T], T)) => (l._1.entityIs(l._2)).execute.properties.filter(_.isOk).size
  lazy val matches = bestMatch(expectedLines, bag, 
                       edgeFunction, 
                       edgeWeight)
  def matchedLines = matches.map(_._3).map((t: (EntityLineForm[T], T)) => (t._1:EntityLineForm[T]).entityIs(t._2).execute)
  def matchedExpectedLines = matches.map(_._1)
  def matchedActual = matches.map(_._2)
  def unmatchedExpectedLines = expectedLines.subtract(matchedExpectedLines)
  def unmatchedActual = bag.toList.subtract(matchedActual)

  private def addLines(lines: List[LineForm]): this.type = {
    lines.foreach { line => 
      if (unsetHeader) {
        setHeader(line)
        unsetHeader = false
      }
      trs(line.rows)
    }
    this
  }
}
