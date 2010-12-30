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
package org.specs.util

/**
 * This trait adds the possibility to declare some elements as included or excluded.
 * 
 * A filter function is then able to return the list of filtered values for a given set of values
 * according to the values which should be included and the values which should be excluded.
 * 
 * The rule is:
 * 
 * - if there are no included values, the value must not be excluded to be one of the filtered values
 * - if there are included values, the values must both be in the included list and not be in the excluded list to 
 *   be part of the filtered values
 */
trait IncludeExclude[T] {
  private var excluded: List[T] = Nil
  private var included: List[T] = Nil
  def exclude(ex: T*): this.type = { excluded = excluded ::: ex.toList; this }
  def include(in: T*): this.type = { included = included ::: in.toList; this }
  def reset(): this.type = { 
    included = Nil 
    excluded = Nil 
    this
  }
  def filter(values: Seq[T]): Seq[T] = values.filter { v => includeCheck(included, v) && excludeCheck(excluded, v) }
  protected def includeCheck(includedValues: List[T], value: T) = includedValues.isEmpty || includedValues.contains(value) 
  protected def excludeCheck(excludedValues: List[T], value: T) = !excluded.contains(value) 
}
/**
 * Default class for the Include/Exclude behaviour
 */
class DefaultIncludeExclude[T] extends IncludeExclude[T]