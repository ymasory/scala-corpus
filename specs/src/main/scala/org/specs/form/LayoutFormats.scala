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
import org.specs.xml.Xhtml._
import org.specs.xml.NodeFunctions._
import scala.xml._
import org.specs.execute._

/**
 * The LayoutFormats trait provides pre-defined formatting rows for headers and paragraphs
 */
trait LayoutFormats extends Layout with Tabs {
  /** empty string property which can be used to display blank lines. */
  protected val empty = Prop[String]("")
  /** display a "paragraph" = an empty row + the values */
  def p(values: LabeledXhtml*): this.type = { tr(empty); tr(values:_*) }
  /** display a big header as a box inside a row */
  def th1(titles: String*): this.type = {
    embedInNewRow(<table class="dataTable"><tr>{titles.map((title:String) => <th>{ removeUnnecessaryNewlines(title) }</th>)}</tr></table>)
  } 
  /** display a big header as a box inside a row */
  def th1(title: LabeledXhtml): this.type = {
    embedInNewRow(<table class="dataTable"><tr>{ title.boldLabels.toXhtml }</tr></table>) 
  }
  /** display a th header */
  def th2(titles: String*): this.type = {
    inNewRow(reduce[String](titles, (title: String) => <th>{ removeUnnecessaryNewlines(title) }</th>))
  }
  /** display a th header */
  def th2(titles: List[LabeledXhtml]): this.type = {
    inNewRow(reduce[LabeledXhtml](titles, (title: LabeledXhtml) => <th>{ title.toXhtml }</th>))
  }
  /** display a th header, left aligned */
  def th3(titles: String*): this.type = {
    inNewRow(reduce[String](titles, (title: String) => <th align="left">{ removeUnnecessaryNewlines(title) }</th>))
  }
  /** display a th header, left aligned */
  def th3(titles: List[LabeledXhtml]): this.type = {
    inNewRow(reduce[LabeledXhtml](titles, (title: LabeledXhtml) => <th align="left">{ title.toXhtml }</th>))
  }
  /** display a th header, left aligned, with a given class attribute */
  def th3(title: String, status: Status.Value): this.type = th3(List(title), status)
  /** display a th header, left aligned, with a success style */
  def th3Success(title: String): this.type = th3(List(title), Status.Success)
  /** display a th header, left aligned, with a failure style */
  def th3Failure(title: String): this.type = th3(List(title), Status.Failure)
  /** display a th header, left aligned, with an error style */
  def th3Error(title: String): this.type = th3(List(title), Status.Error)
  /** display a th header, left aligned, with a Skipped style */
  def th3Skipped(title: String): this.type = th3(List(title), Status.Skipped)
  /** display a th header, left aligned, with an Info style */
  def th3Info(title: String): this.type = th3(List(title), Status.Info)
  /** display a th header, left aligned, with a Success style */
  def th3Success(titles: List[String]): this.type = th3(titles, Status.Success)
  /** display a th header, left aligned, with a Failure style */
  def th3Failure(titles: List[String]): this.type = th3(titles, Status.Failure)
  /** display a th header, left aligned, with an Error style */
  def th3Error(titles: List[String]): this.type = th3(titles, Status.Error)
  /** display a th header, left aligned, with a Skipped style */
  def th3Skipped(titles: List[String]): this.type = th3(titles, Status.Skipped)
  /** display a th header, left aligned, with an Info style */
  def th3Info(titles: List[String]): this.type = th3(titles, Status.Info)
  /** display a list of th headers, left aligned, with a given class attribute */
  def th3(titles: List[String], status: Status.Value): this.type = {
    inNewRow(reduce[String](titles, (title: String) => <th align="left" class={status.toString}>{ removeUnnecessaryNewlines(title) }</th>))
  }
  
  /** remove unnecessary newlines which will cause <p/> to be inserted by markup languages */
  private def removeUnnecessaryNewlines(s: String) = s.replace("\n\n", "\n")
  /** add a new Xhtml element on a new row */
  protected def embedInNewRow(nodes: NodeSeq): this.type = {
    rowValues.append(List(new LabeledXhtml { 
         val label = "none"
         override def toEmbeddedXhtml = <td valign={valignment()} class={statusCode()}>{toXhtml}</td>
         override def toXhtml = nodes
       }
    )) 
    this
  }
  /** add a new Xhtml element on a new row, with no embedding in a separate <td> */
  protected def inNewRow(nodes: NodeSeq): this.type = {
    rowValues.append(List(new LabeledXhtml { 
         val label = "none"
         override def toEmbeddedXhtml = toXhtml
         override def toXhtml = nodes
       }
    ))
    this
  }
}
