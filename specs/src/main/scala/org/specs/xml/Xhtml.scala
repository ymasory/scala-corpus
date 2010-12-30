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
import scala.xml._
import scala.math._

/**
 * utility functions on Xhtml elements.
 * 
 * - spanLastTd: adds a col span on the last td or th element of each row so that it spans the entire table
 * - maxColSize: finds the maximum number of columns in a table
 */
trait Xhtml { outer =>
  /** add a colspan on the last td or th of table rows, equal to the maximum number of columns. */
  def spanLastTd(nodes: NodeSeq): NodeSeq = spanLastTd(nodes, maxColSize(nodes))

  private def spanLastTd(nodes: NodeSeq, spanSize: Int): NodeSeq = {
    nodes.toList match {
      case List(<th>{ b }</th>) => <th colspan={spanSize.toString}>{b}</th> % nodes.toList.head.attributes
      case List(<td>{ b }</td>) => <td colspan={spanSize.toString}>{b}</td> % nodes.toList.head.attributes
      case List(<td>{ b }</td>, Text(x)) => <td colspan={spanSize.toString}>{b}</td> % nodes.toList.head.attributes ++ Text(x)
      /** don't set a colspan on the last cell of the biggest row */
      case <th>{ b }</th> :: otherThs if (nodes.toList.size < spanSize) => nodes.toList.head ++ spanLastTd(otherThs, spanSize)
      case <td>{ b }</td> :: otherTds if (nodes.toList.size < spanSize) => nodes.toList.head ++ spanLastTd(otherTds, spanSize)
      case List(<table>{ x @ _*}</table>) => <table>{spanLastTd(x, spanSize)}</table> % nodes.toList.head.attributes
      case <tr>{ y @ _*}</tr> :: otherRows => <tr>{spanLastTd(y, spanSize)}</tr> ++ spanLastTd(otherRows, spanSize)
      case Text(x) :: other => Text(x) ++ spanLastTd(other, spanSize)
      case other => other
    }
  }
  /** @return the maximum number of columns in a table. */
  def maxColSize(nodes: NodeSeq): Int = maxColSize(nodes, 0)

  /** @return the maximum number of columns in a table, given a previously computed maximum */
  private def maxColSize(nodes: NodeSeq, maximum: Int): Int = {
    nodes.toList match {
      case List(<td>{ b @ _* }</td>) => maximum + 1
      case List(<td>{ b @ _* }</td>, Text(x)) => maximum + 1
      case <td>{ b @ _* }</td> :: others => maxColSize(others, maximum + 1)

      case List(<th>{ b @ _* }</th>) => maximum + 1
      case List(<th>{ b @ _* }</th>, Text(x)) => maximum + 1
      case <th>{ b @ _* }</th> :: others => maxColSize(others, maximum + 1)

      case List(<table>{ x @ _*}</table>) => maxColSize(x, maximum)
      case <tr>{ y @ _*}</tr> :: otherRows => max(maxColSize(y, maximum), maxColSize(otherRows, maximum))
      case Text(x) :: other => maxColSize(other, maximum)
      case other => maximum
    }
  }
  
  /** @return an unsorted html list from a list of objects */
  def itemize[T](list: T*)= {
    list.map("<li>"+_).mkString("<ul>\n", "</li>\n", "</li></ul>\n")
  }
  
  /** include a NodeSeq in a collapsible section with a title */
  def collapsible(title: String, content: NodeSeq): String = collapsible(title, content.toString)
  /** include a String in a collapsible section with a title */
  def collapsible(title: String, content: String): String = collapsible(System.nanoTime, title, content)
  /** create a collapsible section with a given id for a NodeSeq */
  private[specs] def collapsible(id: Long, title: String, content: NodeSeq): String = collapsible(id, title, 5, content.toString)
  /** create a collapsible section with a given id for a String */
  private[specs] def collapsible(id: Long, title: String, content: String): String = collapsible(id, title, 5, content)
  /** create a collapsible section with a given id for a String and a specific header type */
  private[specs] def collapsible(id: Long, title: String, titleNumber: Int, content: String): String = {
    "<div>\n"+
    "<h"+titleNumber+"><img src=\"images/collapsed.gif\" onclick=\"{toggleImage(this); showHideTable('id:" + id + "')}\"/>"+title+"</h"+titleNumber+">\n"+
    "<div id=\"id:"+ id+"\" style=\"display:none\">"+content+"</div>\n"+
    "</div>"
  }
  /**
   * implicit to allow any piece of xml to be transformed to a collapsible section
   */
  implicit def toXhtmlCollapsible(content: => { def toXhtml: NodeSeq }) = new Collapsible(content.toXhtml.toString)
  /**
   * implicit to allow any object with a toString method to be transformed to a collapsible section
   */
  implicit def toCollapsible(content: =>Any) = new Collapsible(content.toString)
  /**
   * implicit class holding the "collapsible" method
   */
  class Collapsible(content: =>String) {
    def collapsible(title: String) = outer.collapsible(title, content)
  }
}
object Xhtml extends Xhtml

