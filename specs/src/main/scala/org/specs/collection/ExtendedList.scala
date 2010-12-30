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
import scala.collection.immutable._
/**
 * This object provides useful functions for Lists, like:<ul>
 * <li><code>everyOrder</code>: return all permutations of the list elements 
 * <li><code>mix</code>: from one list and one element, return all the lists created by inserting this element in the original list
 * <li><code>removeFirst</code>: return a list minus the first element matching a given element according to a predicate
 * </ul>
 */
object ExtendedList { outer =>
  /**
   * @return a list of lists where x is inserted between every element of the original list
   */
  def mix[T](x: T, l: List[T]): List[List[T]] = {
    l match {
      case Nil => List(List(x))
      case y::Nil => List(x::y::Nil, y::x::Nil)
      case y::rest => List(x::l):::mix(x, rest).map((s: List[T]) => y::s)
    }
  }
  /**
   * @return a list of lists containing permutations of the initial list
   */
  def everyOrder[T](l: List[T]): List[List[T]] = {
    l match {
      case Nil => Nil
      case x::Nil => List(l)
      case x::rest => everyOrder(rest).flatMap {mix(x, _)}
    }
  }
  /** @return an ExtendedList object with more functionalities */
  implicit def listToExtendedList[T](l: List[T]) = new ExtendedList(l)
   /**
   * See the description of the ExtendedList object
   */
  class ExtendedList[T](l: List[T]) {
    /**
     * remove the first element satifying the predicate
     * @return a list minus the first element satisfying the predicate
     */
    def removeFirst(predicate: T => Boolean): List[T] = {
      l match {
        case Nil => Nil
        case x::rest if (predicate(x)) => rest
        case x::rest if (!predicate(x)) => List(x):::rest.removeFirst(predicate)
        case _ => Nil // should never happen thanks to the predicate condition above
      }
    }
    /**
     * remove the first occurence of <code>sublist</code> from this list
     * @return a list minus the first occurence of a sublist
     */
    def removeFirstSeq(sublist: List[T]): List[T] = {
      l match {
        case Nil => Nil
        case list if (list.slice(0, sublist.size) == sublist) => list.slice(sublist.size, list.size).toList
        case x::rest => x::rest.removeFirstSeq(sublist)
      }
    }

    /**
     * @return all the prefixes of a list: [1, 2, 3].prefixes == [[1], [1, 2], [1, 2, 3]]
     */
    def prefixes: List[List[T]] = {
      l match {
        case Nil => Nil
        case x::Nil => List(l)
        case x::rest => List(x)::rest.prefixes.map(x::_)
      }
    }

    /**
     * @param defaultValue default value for the map
     * @return a map where the keys are the list elements and the values are set to <code>defaultValue</code>
     */
    def toMap[D](defaultValue: D): scala.collection.immutable.Map[T, D] = {
      var newMap: scala.collection.immutable.Map[T, D] = new HashMap[T, D]
      l.foreach { t:T => newMap = newMap.updated(t, defaultValue)}  
      newMap
    }

    /**
     * @return a randomly mixed list
     */
    def scramble = l.sortWith((a, b) => (new java.util.Random).nextInt(1) > 0)
    
    /**
     * @return a list without duplicates
     */
    def unique = l.distinct
    
    /**
     * @return the max element according the function f
     */
    def maxElement(f: T => Int): Option[T] = outer.maxElement(l, f)
    /**
     * @return the maximum value according the function f
     */
    def maximum(f: T => Int): Int = outer.maximum(l, f)
    /**
     * @return the max according the function f
     */
    def max(f: T => Int) = outer.max(l, f)
    /**
     * @return the min element according the function f
     */
    def minElement(f: T => Int): Option[T] = outer.minElement(l, f)
    /**
     * @return the minimum value according the function f
     */
    def minimum(f: T => Int): Int = outer.minimum(l, f)
    /**
     * @return the min according the function f
     */
    def min(f: T => Int) = outer.min(l, f)
    /** 
     * @return the difference of 2 lists but only removing elements once 
     */
    def subtract(list2: List[T]): List[T] = outer.subtract(l, list2)
  }
  /** 
   * @return the difference of 2 lists but only removing elements once 
   */
  def subtract[A](list1: List[A], list2: List[A]): List[A] = {
    list1 match {
      case Nil => Nil
      case a :: Nil => if (list2.contains(a)) Nil else list1
      case a :: rest => {
        if (list2.isEmpty)
          list1
        else if (list2.exists((x: A) => a == x)) 
          subtract(rest, subtract(list2, List(a))) 
        else 
          a :: subtract(rest, list2)
      } 
    }
  }
  /** @return the maximum element of a list according to a function f returning an Int value for each element of the list */
  def maxElement[T](list: List[T], f: T => Int): Option[T] = max(list, f)._1
  /** @return the maximum value of a list according to a function f returning an Int value for each element of the list */
  def maximum[T](list: List[T], f: T => Int): Int = max(list, f)._2
  /** @return the maximum element and maximum value of a list according to a function f returning an Int value for each element of the list */
  def max[T](list: List[T], f: T => Int): (Option[T], Int) = optimum(list, f, (_>_), 0, None)
  /** @return the minimum element of a list according to a function f returning an Int value for each element of the list */
  def minElement[T](list: List[T], f: T => Int): Option[T] = min(list, f)._1
  /** @return the minimum value of a list according to a function f returning an Int value for each element of the list */
  def minimum[T](list: List[T], f: T => Int): Int = min(list, f)._2
  /** @return the minimum element and maximum value of a list according to a function f returning an Int value for each element of the list */
  def min[T](list: List[T], f: T => Int): (Option[T], Int) = optimum(list, f, (_<_), scala.Int.MaxValue, None)
  /**
   * Find the optimum element of a list according to a valuation function and a comparison function to determine if we require
   * the least or best values.
   * @param list list of elements
   * @param f optimization function returning a value for each element of the list
   * @param compare comparison function indicating how values should be compared to get the optimum. 
   *  For example (_>_) will get the maximum
   * @param currentOptimum current optimum value. This can be 0 when getting the maximum
   * @param optimumElement current optimum element if found. This is None when starting the computation
   * @return the optimum element and value
   */
  def optimum[T](list: List[T], f: T => Int, compare: (Int, Int) => Boolean, currentOptimum: Int, optimumElement: Option[T]): (Option[T], Int) ={
    list match {
      case Nil => (optimumElement, currentOptimum)
      case a :: rest => {
        val currentValue = f(a)
        if (compare(currentValue, currentOptimum)) 
          optimum(rest, f, compare, currentValue, Some(a)) 
        else 
          optimum(rest, f, compare, currentOptimum, optimumElement)
      }
    }
  }
}
