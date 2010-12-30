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
import org.specs.collection.ExtendedList._
import scala.collection.mutable.ListBuffer

/**
 * The ExtendedIterable object offers utility methods applicable to iterables like:<ul>
 * <li><code>toStream</code>
 * <li><code>toDeepString</code>: calls toString recursively on the iterable elements
 * <li><code>sameElementsAs</code>: compares 2 iterables recursively
 * </ul>
 */
object ExtendedIterable {
  /**
   * implicit definition to transform an iterable to an ExtendedIterable
   */
  implicit def iterableToExtended[A](xs : Iterable[A]) = new ExtendedIterable(xs)

  /**
   * See the description of the ExtendedIterable object
   */
  class ExtendedIterable[A](xs:Iterable[A]) {
    /**
     * @return a Stream created from the iterable
     */
    def toStream = xs.iterator.toStream
    /**
     * alias for any type of Iterable
     */
    type anyIterable = Iterable[T] forSome { type T }
    /**
     * @return the representation of the elements of the iterable using the toString method recursively
     */
    def toDeepString: String = {
      if (!xs.isEmpty && xs == xs.iterator.next)
        xs.toString
      else
        "[" + xs.toList.map { x => 
		    x match {
			  case i: Iterable[_] => i.toDeepString
			  case _ => x.toString
			}
        }.mkString(", ") + "]"
    }
    /**
     * @return true if the 2 iterables contain the same elements, in the same order, according to a function f
     */
    def isSimilar[B >: A](that: Iterable[B], f: Function2[A, B, Boolean]): Boolean = {
      val ita = xs.iterator
      val itb = that.iterator
      var res = true
      while (res && ita.hasNext && itb.hasNext) {
        res = f(ita.next, itb.next)
      }
      !ita.hasNext && !itb.hasNext && res
    }
    /**
     * @return true if the second iterable elements are contained in the first, in order
     */
    def containsInOrder[A](l: Iterable[A]): Boolean = {
      val firstList = xs.toList
      val secondList = l.toList
      (firstList, secondList) match {
         case (_, Nil) => true
         case (Nil, _) => false
         case (a :: Nil, b :: Nil) => a == b
         case (a :: firstRest, b :: secondRest) => {
           if (a != b)
             firstRest.containsInOrder(secondList)
           else
             firstRest.containsInOrder(secondRest)
         }
      }
    }
    /**
     * @return true if the 2 iterables contain the same elements recursively, in any order
     */
    def sameElementsAs(that: Iterable[A]): Boolean = sameElementsAs(that, (x, y) => x == y)
    /**
     * @return true if the 2 iterables contain the same elements (according to a comparision function f) recursively, in any order
     */
    def sameElementsAs(that: Iterable[A], f: (A, A) => Boolean): Boolean = {
      def isNotItsOwnIterable(a: Iterable[_]) = a.isEmpty || a.iterator.next != a
	  def matchTwo(x: A, y: A): Boolean = {
		(x, y) match {
		  case (a: Iterable[_], b:Iterable[_]) if (isNotItsOwnIterable(a)) => x.asInstanceOf[Iterable[A]].sameElementsAs(y.asInstanceOf[Iterable[A]], f)
		  case _ => f(x, y)
		}
	  }
      val ita = xs.iterator.toList
      val itb = that.iterator.toList
      var res = true
      (ita, itb) match {
        case (Nil, Nil) => true
        case (a: Iterable[_], b: Iterable[_]) => {
          if (a.headOption.isDefined && b.headOption.isDefined) {
            val (x, y, resta, restb) = (a.head, b.head, a.drop(1), b.drop(1))
            matchTwo(x, y) && resta.sameElementsAs(restb, f) ||
            resta.exists(matchTwo(_, y)) && restb.exists(matchTwo(_, x)) &&
              resta.removeFirst(matchTwo(_, y)).sameElementsAs(restb.removeFirst(matchTwo(_, x)), f)
          }
          else
            false
        }
        case _ => ita == itb
      }
    }
    /**
     * adds the sameElementsAs method to any object in order to do that comparison recursively
     */
    implicit def anyToSameElements(x: Any) = new AnyWithSameElements(x)
    /**
     * Class adding the <code>sameElementsAs</code> method to any object. The default implementation uses standard equality (==)
     */
    class AnyWithSameElements(x: Any) {
      def sameElementsAs(that: Any): Boolean = x == that
    }
  }
}
