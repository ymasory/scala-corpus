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
package org.specs.xml
import org.specs.collection.ExtendedIterable._
import org.specs.xml.NodeFunctions._
import scala.xml._

/**
 * This class adds more methods to the NodeSeq class
 */
class ExtendedNodeSeq(ns: NodeSeq) {
    def ==/(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoringSpace(ns, n)
    def isEqualIgnoringSpace(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoringSpace(ns, n)
    def isEqualIgnoringSpaceOrdered(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoringSpaceOrdered(ns, n)
}

/**
 * This class adds more methods to the Node class
 */
class ExtendedNode(n: Node) {
  /**
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def isSpaceNode: Boolean = NodeFunctions.isSpaceNode(n)
}

/**
 * This object provides implicit methods to extend Node and NodeSeq objects
 */
object ExtendedNode {
  implicit def toExtendedNodeSeq(n: NodeSeq) = new ExtendedNodeSeq(n)
  implicit def toExtendedNode(n: Node) = new ExtendedNode(n)
}

/**
 * This object provides useful functions for Nodes and NodeSeqs
 */
object NodeFunctions {
  /**
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def isSpaceNode(n1: Node): Boolean = n1 match {
    case g: Group => false
    case _ => n1.label.equals("#PCDATA") && n1.text.matches("\\s*")
  }

  /**
   * Alias for isEqualIgnoringSpace
   */
  def ==/(node: NodeSeq, n: NodeSeq): Boolean = isEqualIgnoringSpace(node, n)

  /**
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def isEqualIgnoringSpaceOrdered(node: NodeSeq, n: NodeSeq): Boolean = {
    def sameOrder(nodes1: NodeSeq, nodes2: NodeSeq) = nodes1.isSimilar(nodes2, isEqualIgnoringSpace _)
    isEqualIgnoringSpace(node, n, sameOrder(_, _))
  }
  /**
   * Generic version of This version don't check if the nodes are in the same order
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def isEqualIgnoringSpace(node: NodeSeq, n: NodeSeq): Boolean = {
    def sameAs(nodes1: NodeSeq, nodes2: NodeSeq) = nodes1.sameElementsAs(nodes2.toSeq, isEqualIgnoringSpace _)
    isEqualIgnoringSpace(node, n, sameAs(_, _))
  }
  def isEqualIgnoringSpace(node: NodeSeq, n: NodeSeq, iterableComparison: Function2[NodeSeq, NodeSeq, Boolean]): Boolean = {
    (node, n) match {
      case (null, other) => other == null
      case (other, null) => other == null
      case (n1: Text, n2:Text) => n1.text.trim == n2.text.trim
      case (n1: Text, n2:Atom[_]) => n1.text.trim == n2.text.trim
      case (n1: Atom[_], n2:Text) => n1.text.trim == n2.text.trim
      case (n1: Node, n2:Node) => (isSpaceNode(n1) && isSpaceNode(n2)) ||
                                  n1.prefix == n2.prefix && 
                                  n1.attributes.toSet == n2.attributes.toSet && 
                                  n1.label == n2.label && 
                                  iterableComparison(n1.child.filter(!isSpaceNode(_)), n2.child.filter(!isSpaceNode(_)))
      case (n1: NodeSeq, n2: NodeSeq) => iterableComparison(n1.filter(!isSpaceNode(_)), n2.filter(!isSpaceNode(_)))
    }
  }
  def reduce[T](list: Seq[T], f: T => NodeSeq): NodeSeq = {
    if (list.isEmpty)
      NodeSeq.Empty
    else if (list.size == 1)
      f(list(0))
    else
      f(list(0)) ++ reduce(list.drop(1), f)
  }
  /** reduce a list with a function and an init NodeSeq value. */
  def fold[T](initValue: NodeSeq)(list: Iterable[T], f: T => NodeSeq): NodeSeq = {
    list.foldLeft(initValue)( (res, value) => res ++ f(value))
  }
}