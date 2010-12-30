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
 * The Products object allows the following declarations:<ul>
 *   <li>val list0 = ()
 *   <li>val list1 = ("a")
 *   <li>val list2 = ("a", "b")</ul>
 */
object Products extends Products
/**
 * The Products trait allows the following declarations:<ul>
 *   <li>val list0 = ()
 *   <li>val list1 = ("a")
 *   <li>val list2 = ("a", "b")</ul>
 *  Produced with the following ruby code:<pre>
 *   (1..19).each do |i|
 *     puts "implicit def productToList#{i}[T](t: Product#{i}[#{(1..i).map{|x|"T"}.join(", ")}]) = List(#{(1..i).map {|j| "t._" + j.to_s}.join(", ")})"
 *    end
 *   </pre>
 */
trait Products {
  implicit def unitToList(a: Unit) = List()
  implicit def anyToList[T](a: T) = List[T](a)
  implicit def productToList1[T](t: Product1[T]) = List(t._1)
  implicit def productToList2[T](t: Product2[T, T]) = List[T](t._1, t._2)
  implicit def productToList3[T](t: Product3[T, T, T]) = List(t._1, t._2, t._3)
  implicit def productToList4[T](t: Product4[T, T, T, T]) = List(t._1, t._2, t._3, t._4)
  implicit def productToList5[T](t: Product5[T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5)
  implicit def productToList6[T](t: Product6[T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6)
  implicit def productToList7[T](t: Product7[T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7)
  implicit def productToList8[T](t: Product8[T, T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)
  implicit def productToList9[T](t: Product9[T, T, T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
  implicit def productToList10[T](t: Product10[T, T, T, T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)
  implicit def productToList11[T](t: Product11[T, T, T, T, T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)
  implicit def productToList12[T](t: Product12[T, T, T, T, T, T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)
  implicit def productToList13[T](t: Product13[T, T, T, T, T, T, T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)
  implicit def productToList14[T](t: Product14[T, T, T, T, T, T, T, T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)
  implicit def productToList15[T](t: Product15[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)
  implicit def productToList16[T](t: Product16[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)
  implicit def productToList17[T](t: Product17[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
  implicit def productToList18[T](t: Product18[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
  implicit def productToList19[T](t: Product19[T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T]) = List(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)

}
trait ListsToTuples {
  implicit def listToProduct2[T](l: List[T]): Product2[T, T] = (l(0), l(1))
  implicit def listToProduct3[T](l: List[T]): Product3[T, T, T] = (l(0), l(1), l(2))
  implicit def listToProduct4[T](l: List[T]): Product4[T, T, T, T] = (l(0), l(1), l(2), l(3))
  implicit def listToProduct5[T](l: List[T]): Product5[T, T, T, T, T] = (l(0), l(1), l(2), l(3), l(4))
  implicit def listToProduct6[T](l: List[T]): Product6[T, T, T, T, T, T] = (l(0), l(1), l(2), l(3), l(4), l(5))
  implicit def listToProduct7[T](l: List[T]): Product7[T, T, T, T, T, T, T] = (l(0), l(1), l(2), l(3), l(4), l(5), l(6))
  implicit def listToProduct8[T](l: List[T]): Product8[T, T, T, T, T, T, T, T] = (l(0), l(1), l(2), l(3), l(4), l(5), l(6), l(7))
  implicit def listToProduct9[T](l: List[T]): Product9[T, T, T, T, T, T, T, T, T] = (l(0), l(1), l(2), l(3), l(4), l(5), l(6), l(7), l(8))
  implicit def listToProduct10[T](l: List[T]): Product10[T, T, T, T, T, T, T, T, T, T] =(l(0), l(1), l(2), l(3), l(4), l(5), l(6), l(7), l(8), l(9))
}
