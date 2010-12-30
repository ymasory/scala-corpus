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
import org.specs.specification.DefaultExpectableFactory
/**
 * A Table form is a set of forms, one form per row.
 * 
 * Each time, the set is passed set of line forms, the first line is used to add a header
 */
class TableForm(title: Option[String]) extends Form(title, new DefaultExpectableFactory {}) with TableFormEnabled {
  def this() = this(None)
  def this(t: String) = this(Some(t))
}
trait TableFormEnabled extends FormEnabled {
  /** this variable becomes false when there is no more need to insert a header row in the table */
  private var unsetHeader = true
  /** automatically transform a value into a Field for easier declaration of tr(...) lines */
  implicit def toField[T](a: T) = Field("", a)
  /**
   * adds properties in a line form 
   */
  override def tr(props: LabeledXhtml*): this.type = addProps(props.toList)
  protected def addProps(props: List[LabeledXhtml]): this.type = {
    props match {
      case List(t: Tabs) => super.tr(t)
      case _ => {
        val lineForm = new LineForm {
          override val lineProperties = { 
            val l = new ListBuffer[LabeledXhtml]()
            l.appendAll(props)
            l 
          }
          props.foreach(p => properties.append(p.asInstanceOf[FormProperty with Copyable[FormProperty]]))

        }
        lineForm.formatterIs(genericFormatter)
        this.tr(lineForm)
      }
    }
    this
  }
  /**
   * adding a new form on a line. If this form is a LineForm, add a new header before, from the LineForm header 
   */
  def tr[F <: Form](line: F): F = {
    line match {
      case l: LineForm => setHeader(l)
      case _ => ()
    }
    
    super.form(line)
    trs(line.rows)
    line
  }
  /**
   * sets a header on the table
   */
  def header(names: String*) = {
    setHeader(new LineForm { names.map(n => field(n, n)) })
  }
  
  def setHeader[F <: LineForm](line: F): F = {
    if (unsetHeader) {
      unsetHeader = false
      inNewRow(line.header) 
    } else {
      unsetHeader = true
    }
    line
  }
}
