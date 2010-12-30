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
package org.specs.collection
import java.util.ArrayList
object JavaCollectionsConversion extends JavaConversions

/**
 * This trait provides some implicit conversions between Java collections and scala.collection.List
 */
trait JavaConversions {
  /**
   * @return the vector elements as a List
   */
  implicit def vectorToList[T](v: java.util.Vector[T]): List[T]= {
    var list: List[T] = List()
    val it: java.util.Iterator[T] = v.iterator
    while (it.hasNext) { list = it.next.asInstanceOf[T]::list}
    list
  }

  /**
   * @return the enumeration elements as a List
   */
  implicit def enumerationToList[T](e: java.util.Enumeration[T]): List[T] = {
    var list = List[T]()
    while (e.hasMoreElements()) { list = e.nextElement::list}
    list.reverse
  }

  /**
   * @return the array elements as a List
   */
  implicit def javaArrayToList[T](array: Array[T]): List[T] = {
    var result = List[T]()
    var i = 0
    if (array == null) return List[T]()
    while (i < array.length) { result = array(i) :: result; i += 1 }
    result
  }
}
