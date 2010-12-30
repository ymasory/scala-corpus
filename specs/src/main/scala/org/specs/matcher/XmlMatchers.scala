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
package org.specs.matcher
import scala.xml._
import scala.xml.NodeSeq._
import xpath._
import org.specs.xml.NodeFunctions._
import org.specs.specification.Result

object StringToElem {
  implicit def toElement(s: String) = new ToElem(s)
  class ToElem(s: String) {def toElem: Elem = Elem(null, s, Null, TopScope)}
}
import StringToElem._
/**
 * The <code>XmlMatchers</code> trait provides matchers which are applicable to xml nodes
 */
trait XmlMatchers extends XmlBaseMatchers with XmlBeHaveMatchers
object XmlBaseMatchers extends XmlBaseMatchers
trait XmlBaseMatchers extends StructuralMatchers {
  
  /**
   * Matches if <code>node</code> is contained anywhere inside the tested node
   */   
  def \\(node: Node): XmlMatcher = new XmlMatcher(List(new PathFunction(node, nodeSearch _)))

  /**
   * Alias for <code>\\(node)</code> with the node label only
   */   
  def \\(label: String): XmlMatcher = \\(label.toElem)
  
  /**
   * Matches if <code>node</code> is contained anywhere inside the tested node and has exactly the <code>attributes</code> names 
   * as names for its attributes
   */   
  def \\(node: Node, attributes: List[String]): XmlMatcher = new XmlMatcher(List(new PathFunction(node, attributes, nodeSearch _)))

  /**
   * Alias for <code>\\(node, attributes)</code> with the node label only
   */   
  def \\(label: String, attributes: List[String]): XmlMatcher = \\(label.toElem, attributes)
  /**
   * Matches if <code>node</code> is contained anywhere inside the tested node and has exactly the <code>attributeValues</code> 
   * as names and values for its attributes
   */   
  def \\(node: Node, attributeValues: Map[String, String]): XmlMatcher = new XmlMatcher(List(new PathFunction(node, attributeValues, nodeSearch _)))
  /**
   * Matches if <code>node</code> is contained anywhere inside the tested node and has exactly the <code>attributeValues</code> 
   * as names and values for its attributes
   */   
  def \\(node: Node, attributeValues: Pair[String, String]*): XmlMatcher = \\(node, Map(attributeValues:_*))
  /**
   * Alias for <code>\\(node, attributeValues)</code> with the node label only
   */   
  def \\(label: String, attributeValues: Map[String, String]): XmlMatcher = \\(label.toElem, attributeValues)
  /**
   * Matches if <code>node</code> is contained anywhere inside the tested node and has exactly the <code>attributeValues</code> 
   * as names and values for its attributes
   */   
  def \\(label: String, attributeValues: Pair[String, String]*): XmlMatcher = \\(label.toElem, Map(attributeValues:_*))
  /**
   * Matches if <code>node</code> is a direct child of the tested node
   */   
  def \(node: Node): XmlMatcher = new XmlMatcher(List(new PathFunction(node, subNodeSearch _)))

  /**
   * Alias for <code>\(node)</code> with the node label only
   */   
  def \(label: String): XmlMatcher = \(label.toElem)

  /**
   * Matches if <code>node</code> is a direct child of the tested node and has exactly the <code>attributes</code> names 
   * as names for its attributes
   */   
  def \(node: Node, attributes: List[String]): XmlMatcher = new XmlMatcher(List(new PathFunction(node, attributes, subNodeSearch _)))

  /**
   * Alias for <code>\(node, attributes)</code> with the node label only
   */   
  def \(label: String, attributes: List[String]): XmlMatcher = \(label.toElem, attributes)

  /**
   * Matches if <code>node</code> is a direct child of the tested node and has exactly the <code>attributeValues</code> 
   * as names and values for its attributes
   */   
  def \(node: Node, attributeValues: Map[String, String]): XmlMatcher = new XmlMatcher(List(new PathFunction(node, attributeValues, subNodeSearch _)))
  /**
   * Matches if <code>node</code> is a direct child of the tested node and has exactly the <code>attributeValues</code> 
   * as names and values for its attributes
   */   
  def \(node: Node, attributeValues: Pair[String, String]*): XmlMatcher = \(node, Map(attributeValues:_*))

  /**
   * Alias for <code>\(node, attributeValues)</code> with the node label only
   */   
  def \(label: String, attributeValues: Map[String, String]): XmlMatcher = \(label.toElem, attributeValues)
  /**
   * Matches if <code>node</code> is a direct child of the tested node and has exactly the <code>attributeValues</code> 
   * as names and values for its attributes
   */   
  def \(label: String, attributeValues: Pair[String, String]*): XmlMatcher = \(label.toElem, Map(attributeValues:_*))
  
  /**
   * Matches if <code>node</code> is equal to the tested node without testing empty text
   * @deprecated use beEqualToIgnoringSpace instead
   */   
  def equalIgnoreSpace(node: Seq[Node]) = new EqualIgnoringSpaceMatcher(node)
  /**
   * Matches if <code>node</code> is equal to the tested node without testing empty text
   */   
  def beEqualToIgnoringSpace(node: Seq[Node]) = new EqualIgnoringSpaceMatcher(node)

  /**
   * Alias for equalIgnoreSpace
   */   
  def ==/(node: Seq[Node]): EqualIgnoringSpaceMatcher = equalIgnoreSpace(node)

  def equalToIgnoringSpace(node: Seq[Node]) = beEqualToIgnoringSpace(node)
  def equalToIgnoringSpace(node: Elem) = beEqualToIgnoringSpace(node)
}
trait XmlBeHaveMatchers { this: XmlBaseMatchers =>
  /** 
   * matcher aliases and implicits to use with BeVerb and HaveVerb
   */
  implicit def toNodeIterableResult(result: Result[Seq[Node]]) = new NodeIterableResultMatcher(result)
  class NodeIterableResultMatcher(result: Result[Seq[Node]]) {
    def equalToIgnoringSpace(node: Seq[Node]) = result.matchWithMatcher(beEqualToIgnoringSpace(node))
  }
  implicit def toElemResult(result: Result[Elem]) = new ElemResultMatcher(result)
  class ElemResultMatcher(result: Result[Elem]) {
    def equalToIgnoringSpace(node: Elem) = result.matchWithMatcher(beEqualToIgnoringSpace(node))
  }
  implicit def toNodeSeqResult(result: Result[NodeSeq]) = new NodeSeqResultMatcher(result)
  class NodeSeqResultMatcher(result: Result[NodeSeq]) {
    def size(n: Int) = result.matchWithMatcher(new SizeMatcher(2))
  }
  implicit def toGroupResult(result: Result[Group]) = new GroupResultMatcher(result)
  class GroupResultMatcher(result: Result[Group]) {
    def size(n: Int) = result.matchWithMatcher(new SizeMatcher(2))
  }
}
/**
 * Matcher for equalIgnoreSpace comparison, ignoring the nodes order
 */   
class EqualIgnoringSpaceMatcher(node: Seq[Node]) extends Matcher[Seq[Node]]  { 
  def apply(n: =>Seq[Node]) = {
   (isEqualIgnoringSpace(node.toList, n.toList), dUnquoted(n) + " is equal to " + node, dUnquoted(n) + " is not equal to " + node) }
  def ordered = new EqualIgnoringSpaceMatcherOrdered(node)
}
/**
 * Matcher for equalIgnoreSpace comparison, considering the node order
 */   
class EqualIgnoringSpaceMatcherOrdered(node: Seq[Node]) extends Matcher[Seq[Node]]  { 
  def apply(n: =>Seq[Node]) = {
   (isEqualIgnoringSpaceOrdered(node.toList, n.toList), dUnquoted(n) + " is equal to " + node, dUnquoted(n) + " is not equal to " + node) }
}

/**
 * The XmlMatcher class matches an xml Node, or a list of Nodes against a list of search functions, which can either search for:<ul>
 * <li/>a given direct child, with its label and/or attributes and/or attributes names and values
 * <li/>a given child, direct or not (maybe deeply nested), with its label and/or attributes and/or attributes names and values
 * </ul>
 * 
 * XmlMatchers can be "chained" by using the \ or the \\ methods. In that case, the resulting matcher has a new
 * search function which tries to match the result of the preceding function. For example<pre>
 * <a><b><c><d></d></c></b></a> must \\("c").\("d")</pre> will be ok.
*/
case class XmlMatcher(functions: List[PathFunction]) extends Matcher[Seq[Node]]() with MatchExactly {
  
  override def exactly = {
    functions.map(_.exactly)
    this
  }
  /**
   * checks that the <code>nodes</code> satisfy the <code>functions</code>
   */
  def apply(n: =>Seq[Node]) = {
    val nodes = n
    val result = checkFunctions(functions, nodes, (true, nodes.toString, nodes.toString))
    (result.success, description.map(_ + " ").getOrElse("") + result.okMessage, description.map(_ + " ").getOrElse("") + result.koMessage) 
  }
  
  /**
   * checks that the <code>nodes</code> satisfy the <code>functions</code>
   * @returns a MatcherResult (status, ok message, ko message)
   */
  def checkFunctions(pathFunctions: List[PathFunction], nodes: Seq[Node], result: MatcherResult): MatcherResult = {
    // return the result if we have a failure or if there are no (or no more) functions to check
    if (!result.success || pathFunctions.isEmpty) 
      return result
    
    // check the rest of the functions, with the nodes returned by the current function
    // and build a MatcherResult being a success if the function retrieves some node
    pathFunctions match {
      case function :: rest => {
         val functionResult = function(nodes) 
         val searched = searchedElements(function)
         checkFunctions(rest, 
                        functionResult, 
                        (result.success && !functionResult.isEmpty, 
                        result.okMessage + (if (result.okMessage == nodes.toString) "" else " and") + " contains " + searched, 
                        result.okMessage + (if (result.okMessage == nodes.toString) "" else " but") + " doesn't contain " + searched))
      }
      case _ => result
    }
  }
  
  /**
   * @returns a string representing the searched nodes, attributes, attribute values
   */
  private[this] def searchedElements(function: PathFunction) = {
      val node = if (function.node.child.isEmpty)
                   function.nodeLabel
                 else
                   function.node.toString
      val attributes = if (function.attributes.isEmpty && function.attributeValues.isEmpty) 
                         "" 
                       else 
                         " with attributes: " + function.searchedAttributes
      node + attributes
  }

  /**
   * @returns a new Matcher which will try to find <code>node</code> as a direct child after using
   * functions to find elements
   */
  def \(node: Node): XmlMatcher = new XmlMatcher(functions:::List(new PathFunction(node, Nil, subNodeSearch _)))

  /**
   * @returns a new Matcher which will try to find <code>node</code> as a child (possibly deeply nested) after 
   * using functions to find elements
   */
  def \\(node: Node): XmlMatcher = new XmlMatcher(functions:::List(new PathFunction(node, Nil, nodeSearch _)))

  /**
   * alias for \ using the node label only 
   */
  def \(label: String): XmlMatcher = \(label.toElem)

  /**
   * alias for \\ using the node label only 
   */
  def \\(label: String): XmlMatcher = \\(label.toElem)
}  

/**
 * This object provides XPath functions in order to use them as parameters
 */
object xpath extends XPathFunctions
trait XPathFunctions {
  type XPathFunction = Function2[Node, String, NodeSeq]
  /**
   * @returns the \ XPath function
   */
  def subNodeSearch(node: Node, label: String) = node \ label  

  /**
   * @returns the \\ XPath function
   */
  def nodeSearch(node: Node, label: String) = node \\ label  
}

/**
 * The PathFunction object encapsulate a search for a node and/or attributes or attributeValues with an XPath function
 * If <code>node</code> has some children, then they are searched using equality
 */
class PathFunction(val node: Node, val attributes: List[String], val attributeValues: Map[String, String], val function: XPathFunction) extends Function1[Seq[Node], Seq[Node]] with XPathFunctions with MatchExactly {

  /**
   * @returns a PathFunction looking for a Node
   */
  def this(n: Node, function: XPathFunction) = this(n, Nil, Map.empty, function)

  /**
   * @returns a PathFunction looking for a Node and its attributes
   */
  def this(n: Node, attributes: List[String], function: XPathFunction) = this(n, attributes, Map.empty, function)

  /**
   * @returns a PathFunction looking for a Node and its attributes and attributes values
   */
  def this(n: Node, attributeValues: Map[String, String], function: XPathFunction) = this(n, Nil, attributeValues, function)

  /**
   * @returns the node if it is found and matching the searched attributes and/or attribute values when specified
   */
  def apply(nodes: Seq[Node]): Seq[Node] = for(n <- nodes;
                                                         found <- function(n, node.label) if (matchNode(found))) 
                                                       yield found 

  /**
   * @returns "subnode" or "node" depending on the type of search a direct child search or a general search
   */
  def nodeLabel: String = (if (!function(<a/>, "a").isEmpty) "node " else "subnode " )+ node.label
  
  /**
   * @returns true if the node found with a label also satisfies the attributes and/or values requirement
   */
  def matchNode(found: Node): Boolean = {
    // returns true if m matches the attribute names or attribute names + values
    def attributesMatch(m: MetaData) = {
      if (!attributes.isEmpty) {
        if (allAttributes)
          m.map((a: MetaData) => a.key).toList.intersect(attributes) == attributes
        else {
          val attributesNames = m.map((a: MetaData) => a.key).toList
          attributes.forall(attributesNames.contains(_))
        }
      }
      else if (!attributeValues.isEmpty) {
        if (allAttributes)
          Map(m.map((a: MetaData) => a.key -> a.value.toString).toList: _*) == attributeValues
        else {
          val attributesNamesAndValues: Map[String, String] = Map(m.map((a: MetaData) => a.key -> a.value.toString).toList: _*)
          attributeValues.forall((pair: (String, String)) =>  attributesNamesAndValues.isDefinedAt(pair._1) && attributesNamesAndValues(pair._1) == pair._2)
        }
      }
      else
        true
    }
    // returns true if the node matches the specified children
    def childrenMatch(n: Node) = {
      if (node.child.isEmpty) 
        true 
      else 
        isEqualIgnoringSpace(fromSeq(n.child), fromSeq(node.child))
    }

    attributesMatch(found.attributes) && childrenMatch(found) 
  }

  /**
   * @returns a string representation of attributes or attributeValues (one of them being empty by construction)
   */
  def searchedAttributes = attributes.mkString(", ") + attributeValues.map(a=> a._1 + "=\"" + a._2 + "\"").mkString(" ")
}
trait MatchExactly {
  /** if allAttributes is true then all the attributes names and values will be checked */
  protected[specs] var allAttributes = false
  /** check that all attribute names and values are indeed present on the found node */
  def exactly: this.type = {
    allAttributes = true
    this
  }
}
