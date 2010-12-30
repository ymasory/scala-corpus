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
import scala.Math._
import org.specs.collection.ExtendedList._
import org.specs.collection.ExtendedIterable._

object Matching extends Matching
trait Matching {
  /**
   * @return a list containing the matched vertices and corresponding edges
   */
  def bestMatch[A, B, E](firstSet: Seq[A], 
                         secondSet: Seq[B], 
                         edgeFunction: Function1[(A, B), E], 
                         edgeWeight: E => Int): List[(A, B, E)] = {
    
    // brutal force approach
    // create all possible combinations and take the least costly
    firstSet match {
      case Nil => Nil
      case a :: rest => {
        bestMatch(a, secondSet, edgeFunction, edgeWeight) match {
          case None => Nil
          case Some((b, e, remainingSecondSet)) => (a, b, e) :: bestMatch(rest, remainingSecondSet, edgeFunction, edgeWeight)
        }
      }
      case _ => Nil
    }
  }
  def bestMatch[A, B, E](a: A, 
                         secondSet: Seq[B], 
                         edgeFunction: Function1[(A, B), E], 
                         edgeWeight: E => Int): Option[(B, E, Seq[B])] = {
    var existingEdges = Map[(A, B), E]()
    def edge(a: A, b: B) = {
      if (existingEdges.isDefinedAt((a, b)))
        existingEdges((a, b))
      else {
        val newEdge = edgeFunction(a, b)
        existingEdges = existingEdges.updated((a, b), newEdge)
        newEdge
      }
    }
    secondSet.toList.maxElement((b: B) => edgeWeight(edge(a, b))) match {
      case None => None
      case Some(max) => Some((max, edge(a, max), secondSet.toList.removeFirst(_ == max)))
    }
  }
}

