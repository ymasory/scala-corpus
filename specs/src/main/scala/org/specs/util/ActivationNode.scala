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
 * This trait is used to tag specifications, systems and examples and give them children/parent/path functionalities
 * This is especially used to find an example in a specification from its path from the root
 */
trait TreeNode extends Tree[TreeNode]
/**
 * This trait declares a generic tree structure with a parent and a list of children.
 */
trait Tree[T <: Tree[T]] {
  private[util] var childrenNodes: List[T] = Nil
  private[util] var parentNode: Option[Tree[T]] = None
  
  /**
   * @return a list of integer representing the path from the root node to this node
   */
  def pathFromRoot: TreePath = parentNode match {
    case None => new TreePath(0)
    case Some(p) => {
      val parentPath = p.pathFromRoot
      val index = p.childrenNodes.zipWithIndex.find { (t: (Tree[T], Int)) => val (n, i) = t
        n eq this
      }.get._2
      parentPath ::: new TreePath(index)
    }
  } 
  /**
   * add a new child and set its parent
   */
  def addChild(t: T) = {
    childrenNodes = childrenNodes ::: List(t)
    t.parentNode = Some(this)
  }
  def childNodes = childrenNodes
}
/**
 * List of nodes representing a path in a tree
 */
case class TreePath(path: List[Int]) {
  def this(i: Int) = this(List(i))
  def :::(other: TreePath) = TreePath(other.path ::: path)
  def isFirst = path.forall(_ == 0)
}
