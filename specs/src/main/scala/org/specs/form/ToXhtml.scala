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
import org.specs.xml.NodeFunctions._
import scala.xml._
/**
 * This trait declares objects with can render themselves as Xhtml
 * 
 */
trait ToXhtml extends DecoratedXhtml {
  /** valign value */
  protected val valignment = Property("top")
  /** set the valign value */
  def valign(s: String): this.type = { valignment(s); this }
  /** status code value */
  protected val statusCode = Property("info")
  /** set the status code */
  def statusClass(s: String): this.type = { statusCode(s); this }
 
  /** @return the Xhtml as a String */
  def toXhtml: NodeSeq = NodeSeq.Empty
  /** @return the Xhtml embedded in a <td> cell with the status code and alignment */
  def toEmbeddedXhtml: NodeSeq = <td valign={valignment()} class={statusCode()}>{ toXhtml }</td>
  /** @return the Html as a String */
  def toHtml: String = {
    val xhtml = toXhtml
    if (xhtml.size == 1)
      new scala.xml.PrettyPrinter(10000, 2).format(xhtml(0))
    else if (xhtml.size > 1)
      new scala.xml.PrettyPrinter(10000, 2).format(Group(xhtml))
    else
      xhtml.toString
  }
  def copy(c: ToXhtml) = {
    c.valignment(valignment())
    c.statusCode(statusCode())
    super.copy(c)
  }
}
trait DecoratedXhtml {
  /** this functions will decorate the labels on the field/property/form */
  protected var labelsDecorators: List[Node => Node] = Nil
  /** appends a new function to decorate the labels on the field/property/form*/
  def decorateLabelsWith(x: Node => Node): this.type = { labelsDecorators = labelsDecorators ::: List(x); this }
  /** appends a new function to decorate the labels on the field/property/form - singular form for fields and properties */
  def decorateLabelWith(x: Node => Node): this.type = decorateLabelsWith(x)
  /** this functions will decorate the values on the field/property/form*/
  protected var valuesDecorators: List[Node => Node] = Nil
  /** appends a new function to decorate the values on the field/property/form*/
  def decorateValuesWith(x: Node => Node): this.type = { valuesDecorators = valuesDecorators ::: List(x); this }
  /** appends a new function to decorate the values on the field/property/form - singular form for fields and properties*/
  def decorateValueWith(x: Node => Node): this.type = decorateValuesWith(x)
  /** this functions will decorate the value cell on the field/property/form*/
  protected var valuesCellsDecorators: List[Node => Node] = Nil
  /** appends a new function to decorate the cell values on the field/property/form - singular form for fields and properties*/
  def decorateValuesCellsWith(x: Node => Node): this.type = { valuesCellsDecorators = valuesCellsDecorators ::: List(x); this }
  /** appends a new function to decorate the cell values on the field/property/form - singular form for fields and properties*/
  def decorateValueCellWith(x: Node => Node): this.type = decorateValuesCellsWith(x)
  /** this functions will decorate the label cell on the field/property/form*/
  protected var labelsCellsDecorators: List[Node => Node] = Nil
  /** appends a new function to decorate the label cells on the field/property/form - singular form for fields and properties*/
  def decorateLabelsCellsWith(x: Node => Node): this.type = { labelsCellsDecorators = labelsCellsDecorators ::: List(x); this }
  /** appends a new function to decorate the cell values on the field/property/form - singular form for fields and properties*/
  def decorateLabelCellWith(x: Node => Node): this.type = decorateLabelsCellsWith(x)
  
  def decorateLabel(label: String) = labelsDecorators.foldLeft({Text(label)}: Node) { (res: Node, decorator: Node => Node) => 
     decorator(res)
  }
  def decorateValue(value: String) = valuesDecorators.foldLeft({Text(value)}: Node) { (res: Node, decorator: Node => Node) => 
     decorator(res)
  }
  def decorateLabelCell(labelCell: Node) = labelsCellsDecorators.foldLeft(labelCell) { (res: Node, decorator: Node => Node) => 
     decorator(res)
  }
  def decorateValueCell(valueCell: Node) = valuesCellsDecorators.foldLeft(valueCell) { (res: Node, decorator: Node => Node) => 
     decorator(res)
  }
  private def setAttribute(s: Node, name: String, value: String) = s match {
    case e: Elem => e % new UnprefixedAttribute(name, value, Null)
    case _ => s
  }
  def valueCellAttribute(name: String, value: String): this.type = decorateValuesCellsWith((s:Node) => setAttribute(s, name, value))
  def labelCellAttribute(name: String, value: String): this.type = decorateLabelsCellsWith((s:Node) => setAttribute(s, name, value))
  def valueBgcolor(value: String): this.type = valueCellAttribute("bgcolor", value).valueCellAttribute("class", "none")
  def labelBgcolor(value: String): this.type = labelCellAttribute("bgcolor", value).labelCellAttribute("class", "none")
  def successValues: this.type = valueCellAttribute("class", "success")
  def successValue: this.type = successValues
  def successLabels: this.type = labelCellAttribute("class", "success")
  def successLabel: this.type = successLabels
  def failureValues: this.type = valueCellAttribute("class", "failure")
  def failureValue: this.type = failureValues
  def failureLabels: this.type = labelCellAttribute("class", "success")
  def failureLabel: this.type = failureLabels
  def errorValues: this.type = valueCellAttribute("class", "error")
  def errorValue: this.type = errorValues
  def errorLabels: this.type = labelCellAttribute("class", "error")
  def errorLabel: this.type = errorLabels
  def skippedValues: this.type = valueCellAttribute("class", "skipped")
  def skippedValue: this.type = skippedValues
  def skippedLabels: this.type = labelCellAttribute("class", "skipped")
  def skippedLabel: this.type = skippedLabels
  def infoValues: this.type = valueCellAttribute("class", "info")
  def infoValue: this.type = infoValues
  def infoLabels: this.type = labelCellAttribute("class", "info")
  def infoLabel: this.type = infoLabels

  def preValues: this.type = decorateValuesWith((s:Node) => <pre>{s}</pre>)
  def preValue: this.type = preValues
  def preLabels: this.type = decorateLabelsWith((s:Node) => <pre>{s}</pre>)
  def preLabel: this.type = preLabels
  def pre: this.type = { preLabels.preValues }
  def italicValues: this.type = decorateValuesWith((s:Node) => <i>{s}</i>)
  def italicValue: this.type = italicValues
  def italicLabels: this.type = decorateLabelsWith((s:Node) => <i>{s}</i>)
  def italicLabel: this.type = italicLabels
  def italic: this.type = { italicLabels.italicValues }
  def boldValues: this.type = decorateValuesWith((s:Node) => <b>{s}</b>)
  def boldValue: this.type = boldValues
  def boldLabels: this.type = decorateLabelsWith((s:Node) => <b>{s}</b>)
  def boldLabel: this.type = boldLabels
  def bold: this.type = { boldLabels.boldValues }
  def strikeValues: this.type = decorateValuesWith((s:Node) => <s>{s}</s>)
  def strikeValue: this.type = strikeValues
  def strikeLabels: this.type = decorateLabelsWith((s:Node) => <s>{s}</s>)
  def strikeLabel: this.type = strikeLabels
  def strike: this.type = { strikeLabels.strikeValues }

  def copy(c: DecoratedXhtml) = {
    def copyList[T](l: List[T]) = l.foldLeft(Nil:List[T]) { (res, cur) => cur :: res }.reverse
    c.labelsDecorators = copyList(labelsDecorators)
    c.labelsCellsDecorators = copyList(labelsCellsDecorators)
    c.valuesDecorators = copyList(valuesDecorators)
    c.valuesCellsDecorators = copyList(valuesCellsDecorators)
  }
}