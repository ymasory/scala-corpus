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
import org.specs.util.Property
/**
 * Matcher prop on an Iterable value.
 * This subclass of Prop is able to display its values differently, like one per line.
 */
class MatcherPropIterable[T](override val label: String,
                             expectedIt: Property[Iterable[T]],
                             actual: Property[Iterable[T]], constraint: Option[MatcherConstraint[Iterable[T]]]) extends
  MatcherProp[Iterable[T]](label, expectedIt, actual, constraint) {

  val valuesFormatter = new ValuesFormatter[T] {}
  
  override def copy: MatcherPropIterable[T] = {
    val p = new MatcherPropIterable(label, expectedIt, actual, constraint)
    super[MatcherProp].copy(p)
    valuesFormatter.copy(p.valuesFormatter)
    p
  }
  /**
   * change the value formatter to display the value differently
   */
  override def formatWith(function: Option[Iterable[T]] => String): this.type = { 
    valuesFormatter.formatIterableWith(function)
    super.formatWith(function)
  }
  /**
   * change the value formatter to display the value differently. This formatter displays "" for a missing value
   */
  def formatterIs(function: T => String) = {
    valuesFormatter.formatterIs(function)
    this
  }

  def formatValue(v: T) = {
    valuesFormatter.formatValue(v)
  }
  def formatValueWith(f: Option[T] => String): this.type = {
    valuesFormatter.formatValueWith(f)
    this
  }

  /** apply method with multiple args for better readability */
  def apply(v: T*): this.type = {
    expected(v)
    this
  }
  /**
   * Display the property:
   * 
   * label: "this" (expected: "that")
   */
  override def toString = {
    label + ": " + formatStringValue(this.actual.optionalValue) + " (expected: " + formatStringValue(expected.optionalValue) + ")"
  }
  
  private def formatStringValue(v: Option[Iterable[T]]) = {
    v.map(valuesFormatter.formatIterable(_)).getOrElse("_")
  }
  
  override private[form] def formattedValue = decorateValue(valuesFormatter.formatIterable(expected.getOrElse(actual.getOrElse(Nil: Iterable[T]))))
}
/**
 * Companion object containing default factory methods
 */
case object PropIterable {
  def apply[T](value: =>Iterable[T]): MatcherPropIterable[T] = PropIterable.apply("", value)
  def apply[T](label: String, value: =>Iterable[T]): MatcherPropIterable[T] = new MatcherPropIterable(label, Property[Iterable[T]](), Property(value), None)
  def apply[T](label: String, value: =>Iterable[T], c: MatcherConstraint[Iterable[T]]): MatcherPropIterable[T] = new MatcherPropIterable(label, Property[Iterable[T]](), Property(value), Some(c))
}
