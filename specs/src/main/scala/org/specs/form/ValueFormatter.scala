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

/**
 * Format values using a default formatter (for Doubles) or a custom one (set with the formatWith  function)
 */
trait ValueFormatter[T] extends GenericFormatter {
  /** value formatter. By default formats Doubles with all decimals */
  private[form] var formatter = (t:Option[T]) => t match {
    case Some(d: Double) => 
      val df = new java.text.DecimalFormat("#.###############", java.text.DecimalFormatSymbols.getInstance(java.util.Locale.US))
      genericFormatter(df.format(d))
    case Some(x: Any) => genericFormatter(x.toString)
    case None => ""
  }
  /**
   * format the value. If it is a Double, use a DecimalFormat("#.###############") to display the value
   */
  def format(s: Option[T]): String = formatter(s)
  /**
   * format the value. If it is a Double, use a DecimalFormat("#.###############") to display the value
   */
  def format(s: T): String = format(Some(s))
  /**
   * change the value formatter to display the value differently
   */
  def formatWith(function: Option[T] => String): this.type = { formatter = function; this }
  /**
   * change the value formatter to display the value differently. This formatter displays "" for a missing value
   */
  def formatterIs(function: T => String): this.type = { 
    formatter = (t: Option[T]) => t match {
      case None => ""
      case Some(x) => function(x)
    }
    this 
  }
  def copy(c: ValueFormatter[T]) = {
    c.formatter = this.formatter
  }
}
trait GenericFormatter {
  private[form] var genericFormatter = (s: String) => s
  def genericFormatterIs(function: String => String): this.type = {
    genericFormatter = function
    this
  } 
}
/**
 * Formatter for an Iterable
 */
trait ValuesFormatter[T] {
  /** value formatter. Format decimals properly */
  private var valueFormatter = new ValueFormatter[T] {}.formatter  
  /** values formatter. By default formats lists by inserting commas */
  protected var valuesFormatter = (t:Option[Iterable[T]]) => t match {
    case None => ""
    case Some(x) => x.toList.map((t:T) => formatValue(Some(t))).mkString(", ")
  }
  /**
   * format the value. If it is a Double, use a DecimalFormat("#.###############") to display the value
   */
  def formatValue(s: Option[T]): String = valueFormatter(s)
  /**
   * format the value. If it is a Double, use a DecimalFormat("#.###############") to display the value
   */
  def formatValue(s: T): String = formatValue(Some(s))
  /**
   * format the value. If it is a Double, use a DecimalFormat("#.###############") to display the value
   */
  def formatIterable(s: Option[Iterable[T]]): String = valuesFormatter(s)
  /**
   * format the value. If it is a Double, use a DecimalFormat("#.###############") to display the value
   */
  def formatIterable(s: Iterable[T]): String = valuesFormatter(Some(s))
  /**
   * change the values formatter to display the values differently
   */
  def formatIterableWith(function: Option[Iterable[T]] => String): this.type = { valuesFormatter = function; this }
  /**
   * change the values formatter to display the values differently, using "" for missing values
   */
  def iterableFormatterIs(function: Iterable[T] => String): this.type = { 
    valuesFormatter = (l: Option[Iterable[T]]) => l match {
      case None => ""
      case Some(x) => function(x)
    }
    this 
  }
  /**
   * change the value formatter to display the value differently
   */
  def formatValueWith(function: Option[T] => String): this.type = { valueFormatter = function; this }
  /**
   * change the value formatter to display the value differently. This formatter displays "" for a missing value
   */
  def formatterIs(function: T => String): this.type = { 
    formatValueWith((t: Option[T]) => t match {
      case None => ""
      case Some(x) => function(x)
    })
  }
  def copy(c: ValuesFormatter[T]) = {
    c.valueFormatter = valueFormatter
    c.valuesFormatter = valuesFormatter
  }
}
