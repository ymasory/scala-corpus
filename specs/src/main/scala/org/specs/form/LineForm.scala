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
import scala.collection.mutable._
import scala.xml._
import org.specs.xml.NodeFunctions._
import org.specs.util.Property
import org.specs.execute.DefaultExecutable
/**
 * A LineForm is a set of LineProps or LineFields which are displayed on the same line.
 * 
 * It is meant to be used in a SeqForm or a SetForm.
 * 
 * The field and prop methods are overriden to create and attach LineFields and LineProps to the Form
 *
 */
class LineForm extends Form {
  /** state variable to know if the rows have already been tr-ed to the form. */
  private var propertiesAreLayedout = false
  /** list of the properties to display */
  protected val lineProperties: ListBuffer[LabeledXhtml] = new ListBuffer

  /** add a new LineField to that line */
  override def field[T](label: String, actual: =>T) = {
    val f = new LineField(label, Property(actual))
    lineProperties.append(f)
    f
  }
  /** add a new LineProp to that line */
  override def prop[T](label: String, actual: =>T) = {
    val p = new LineProp(label, Property[T](), Property(actual), Some(new MatcherConstraint(Some(actual), executor)))
    lineProperties.append(p)
    add(p)
    p
  }
  /** add a new LineProp to that line with expected and actual value */
  def prop[T](label: String, expected: Option[T], actual: =>Option[T]): LineProp[T
  ] = {
    val p = new LineProp(label, new Property(() => expected), new Property(() => actual), Some(new MatcherConstraint(actual, executor)))
    lineProperties.append(p)
    add(p)
    p
  }
  /** when rows are requested (one row only in that case), the properties are added on t
   * he same row.  */
  override def rows = {
    if (!propertiesAreLayedout) {
      tr(lineProperties:_*)
      propertiesAreLayedout = true
    }
    super.rows
  }
  /** extract a header from all the property labels */
  def header = {
    val bareHeader = reduce(lineProperties.map(_.label), { (cur: String) => <th>{cur}</th> })
    headerAttributes match {
      case None => bareHeader
      case Some(att) => bareHeader.toList.map {
        case e: Elem => e % att
        case other => other
      } 
    }  
  }
  /** add attributes to the xhtml header */
  private var headerAttributes: Option[MetaData] = None
  def %(attributes: MetaData): this.type = {
    headerAttributes = Some(attributes)
    this
  }
  /** return the xhtml of all properties without the label (because they are LineProp and LineField) */
  override def toXhtml = reduce(lineProperties, { (p: LabeledXhtml) => p.toXhtml })
  /** return the xhtml of all properties without the label (because they are LineProp and LineField) */
  override def toEmbeddedXhtml = toXhtml
  
  override def copy: LineForm = {
    val f = new LineForm
    this.lineProperties.foreach { (p: LabeledXhtml) => 
      f.lineProperties.append(p.copy)
    }
    copyPropertiesAndFields(f)
  }
  override def resetAll(): this.type = {
    super.resetAll()
    lineProperties.clear()
    this
  }
}
object LineForm {
  /** create a LineForm with labels only to create header rows */
  def apply(labels: String*) = new LineForm {
    labels.foreach((s:String) => field(s, s))
  }
}