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

import scala.xml.NodeSeq
import org.specs.execute._
import org.specs.matcher._
import org.specs.util.Property

/**
 * The Prop class is a named property which holds:<ul>
 * <li/>an expected value
 * <li/>an actual value
 * <li/>a constraint
 * </ul>
 *
 * This property can then be executed, its results can be fetched and displayed as Html
 * It has also the possibility of being inserted in the Layout of a Form.
 *
 * A Prop property is meant to be declared as "bound" to an actual value:<code>
 *
 *   val customerName = Prop("Customer name", person.name)
 * </code>
 * Note that the actual value is not evaluated until the Prop is executed.
 * 
 * Then it can be associated an expected value with the apply method (usually in a Form declaration):
 *
 *   customerName("Bill")
 *
 * Different constraints can be set on a Prop, by using the companion object factory methods:<code>
 *
 * // build a Prop with an AnyConstraint. The block will be executed if the property is executed 
 * val propWithABlock = Prop("label", actualValue, thisVariableInTheScope must_== thatVariableInTheScope)
 *
 * // build a Prop with a FunctionConstraint. The function will be executed with the actual and expected values if the property is executed
 * val propWithAFunction = Prop("label", actualValue, (actual, expected) => actual must_== expected)
 *
 * // build a Prop with a MatcherConstraint. The function will be executed with the default matcher (BeEqualTo)
 * //  if the property is executed
 * val propWithAMatcherExecutor = Prop("label", actualValue, m => actual must m)
 *
 * // Note that in that case the matcher set on the constraint can be changed with
 * propWithAMatcherExecutor.matchesWith(beGreaterThan(_))
 *
 * </code>
 * 
 * Props can be temporarily commented out with the comment() method and thus won't be
 * executed:
 *   name("Eric").comment() // won't check the expected value
 *
 * After execution, Props can be queried for their results using the HasResults trait.
 * Then they can be outputed in Html
 *
 */
class Prop[T](val label: String,
              var expected: Property[T],
              var actual: Property[T], constraint: Option[Constraint[T]]) 
              extends DefaultExecutable with LabeledXhtml with ValueFormatter[T] with Copyable[Prop[T]] {
  
  override def copy: Prop[T] = {
    val p = new Prop[T](label, expected, actual, constraint)
    super[ValueFormatter].copy(p)
    p
  }
  def copy(p: Prop[T]) = {
    super[ValueFormatter].copy(p)
    super[LabeledXhtml].copy(p)
  } 
    

  /**
   * The apply method sets the expected value and returns the Prop
   */
  def apply(e: T): Prop[T] = {
    expected(e)
    this
  }
  def actualValue = actual.optionalValue
  /** 
   * shortcut method for this().get returning the contained expected value.
   * @return the expected value if set and throws an exception otherwise
   */
  def get: T = expected.get

  /** execute the constraint set on this property, with the expected value */
  protected def executeThis = constraint.map { c => 
    c.execute(expected.optionalValue)
  }

  /**
   * Display the property:
   * 
   * label: "this" (expected: "that")
   */
  override def toString = {
    label + ": " + this.actual.map(format(_)).getOrElse("_") + " (expected: " + expected.map(format(_)).getOrElse("_") + ")"
  }
  /** format the expected value if set or else the actual value. */
  private[form] def formattedValue = decorateValue(format(expected.orElse(actual).optionalValue))
  /** @return the status of the execution or value if the cell hasn't been executed. */
  override def statusClass = if (executed) super.statusClass else "info"
  /**
   * Display the Property expected value with a different class attribute
   * depending if it hasn't been executed (white), has failed (yellow),
   * isOk (green, yes!)
   */
  override def toXhtml = {
    if (label.isEmpty) 
      decorateValueCell(valueCell)
    else
      decorateLabelCell(<td>{ decorateLabel(label) }</td>) ++ decorateValueCell(valueCell)
  }
  /**
   * execute the Prop and return the Xhtml
   */
  def toXhtml_! = execute.toXhtml
  /**
   * execute the Prop and return the Xhtml
   */
  def display_! = <table class="dataTable"><tr>{ toXhtml_! }</tr></table>.toString
  
  /**
   * @return the formatted value with its status and error messages if any
   */
  private[form] def valueCell = {
    if (executed) {
      if (isOk)
        <td class={statusClass}>{ formattedValue }</td>
      else {
        <td class={statusClass} valign="top"><b>{ formattedValue }</b>
        {issueMessages}
        </td>
      } 
     }
     else
       <td class="info">{ formattedValue }</td>
  }
  
  /**
   * When embedded in an Html table, a Prop doesn't need a new <td/> cell.
   */
  override def toEmbeddedXhtml = toXhtml
}
/**
 * Companion object containing default factory methods
 */
case object Prop {
  def apply[T](label: String, value: =>T, toCheck: => Any): Prop[T] = new Prop(label, Property[T](), Property(value), Some(AnyConstraint(() => toCheck)))
  def apply[T](label: String, value: =>T, f: (T, T) => Any): Prop[T] = new Prop(label, Property[T](), Property(value), Some(FunctionConstraint(value, f)))
  def apply[T](label: String, value: =>T, c: Constraint[T]): Prop[T] = new Prop(label, Property[T](), Property(value), Some(c))
  def apply[T](label: String, value: =>T, c: MatcherConstraint[T]): MatcherProp[T] = new MatcherProp(label, Property[T](), Property(value), Some(c))
  def apply[T](label: String, value: =>T): MatcherProp[T] = new MatcherProp(label, Property[T](), Property(value), None)
  def apply[T](label: String): MatcherProp[T] = new MatcherProp(label, Property[T](), Property[T](), None)
  def apply[T](value: =>T): MatcherProp[T] = new MatcherProp("", Property[T](), Property[T](value), None)
}

/**
 * generic trait for anything having a label, to unify Props and Forms
 */
trait HasLabel {
  val label: String
}