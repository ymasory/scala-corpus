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
import org.specs.util.Property
import org.specs.execute.DefaultExecutable
/**
 * A Field is a property which is used only to display input values or output values.
 * The apply method can be used to retrieve the Fields value:<code>
 *   val f = Field(label, 1)
 *   f() must_== 1
 * </code>
 * 
 * The value is stored in a Property object so it will not be evaluated until explicitly queried.
 */
class Field[T](val label: String, val value: Property[T]) extends LabeledXhtml with ValueFormatter[T] with Copyable[Field[T]] with DefaultExecutable {
  /** @return a copy of this Field with the same value */
  override def copy: Field[T] = {
    val f = new Field(label, value)
    copy(f)
    f
  }
  def copy(f: Field[T]) = {
    super[ValueFormatter].copy(f)
    super[LabeledXhtml].copy(f)
  }
  /** executing a field does nothing */
  override def executeThis = this
  /**
   * set a new value on the field. 
   */
  def apply(v: =>T): this.type = {
    value(v)
    this
  }
  /** @return the field value */
  def apply(): T = value()
  /** alias for apply() */
  def get: T = apply()
  /** @return "label: value" */
  override def toString = label + ": " + this.get
  /** return the value in a <td> cell, formated and decorated */
  protected def valueCell = <td class="info">{ decorateValue(format(this.get)) }</td>
  /** return the value <td> cell with a td cell for the label if it is not empty */
  override def toXhtml = {
    if (label.isEmpty) 
      decorateValueCell(valueCell)
    else
      decorateLabelCell(<td>{decorateLabel(label)}</td>) ++ decorateValueCell(valueCell)
  }
  /** don't add a supplementary <td> when embedding the xhtml */
  override def toEmbeddedXhtml = toXhtml
  /** transforms this typed Field as a Field containing the toString value of the Fields value*/
  def toStringField = Field(label, value.toString)
  
  override def equals(a: Any): Boolean = {
    a match {
      case f: Field[_] => f.label == label && f.value == value
      case _ => false 
    }
  }
  override def hashCode = label.hashCode + value.hashCode
}
/**
 * Factory methods for creating Fields. Fields values can also be concatenated to produce "summary" fields.
 * 
 * val f1 = Field(label, "hello")
 * val f2 = Field(label, "world")
 * val concatenatedFields = Field(label, f1, f2)
 * concatenatedFields.toString == label: hello/world
 * 
 * val concatenatedFields2 = Field(label, ", ", f1, f2)
 * concatenatedFields2.toString == label: hello, world
 */
case object Field {
  def apply[T](value: =>T): Field[T] = new Field("", Property(value))
  def apply[T](label: String, value: =>T): Field[T] = new Field(label, Property(value))
  def apply[T](label: String, value1: Field[T], values: Field[T]*): Field[String] = Field(label, "/", value1, values:_*)
  def apply[T](label: String, separator: String, value1: Field[T], values: Field[T]*): Field[String] = {
    val f = if (values.isEmpty)
      Field(label, value1.get.toString)
    else
      Field(label, (value1 :: values.toList).map(_.get).mkString(separator))
    value1.copy(f)
    f
  }
}

