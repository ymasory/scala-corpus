/* Copyright (c) 2008 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


package com.google.xml.combinators

import java.io.{InputStream, InputStreamReader, FileInputStream}

import scala.xml.{Node, Elem, MetaData, NamespaceBinding, Text, ProcInstr, 
                  Comment, TopScope, Null, XML, parsing, EntityRef, Utility, Atom}
import scala.io.Source

/**
 * This class encapsulate the state carried around
 * when pickling or unpickling XML. This is an immutable data structure.
 * Speaking from the point of view of unpickling, the store consists of a
 * set of attributes not yet consumed, a set of nodes not yet consumed and
 * a set of namespace bindings encountered so far.
 *
 * @author Iulian Dragos (iuliandragos@google.com)
 */
class LinearStore(ats: MetaData, nods: List[Node], bindings: NamespaceBinding) 
    extends XmlInputStore {
  def attrs = ats
  def nodes = nods
  def ns    = bindings
  var skipNonElements = true
  
  /** 
   * Set whitespace handling when looking for elements. Defaults to skipping whitespace, 
   * comments and processing instructions.
   */
  def setSkipNonElements(v: Boolean): this.type = {
    skipNonElements = v
    this
  }
  
  /**
   * Skips whitespace from the list of nodes. Whitespace is considered to be: empty (only
   * space) text nodes, comments and processing instructions. 
   */
  private def doSkipWhitespace: List[Node] = {
    def isWhiteSpace(n: Node): Boolean = n match {
      case Text(str) => str.trim.isEmpty
      case ProcInstr(_, _) | Comment(_) => true
      case _ => false
    }
    
    if (!skipNonElements) nodes
    else {
      var n = nodes
      while (n != Nil && isWhiteSpace(n.head)) { 
        n = n.tail
      }
      n
    }
  }

  /**
   * Accept the given element, or fail. Succeeds when the given element is the head of the node
   * list. Comments, processing instructions and white space are skipped if 'skipsWhitespace' is
   * set (default). 
   */
  def acceptElem(Label: String, uri: String): (Option[Node], XmlInputStore) = {
    val n = doSkipWhitespace
    if (n.isEmpty)
      (None, this)
    else 
      n.head match {
        case e @ Elem(_, Label, _, scope, _*) if (e.namespace ==  uri) => 
          (Some(e), mkState(attrs, n.tail, ns))
        case _ => (None, this)
      }
  }
  
  /**
   * Accept the given prefixed attribute, or fail. Succeeds when the given attribute exists
   * (order does not matter). Returns a Seq[Node], since attributes may contain text nodes 
   * interspersed with entity references.
   */
  def acceptAttr(label: String, uri: String): (Option[Seq[Node]], XmlInputStore) = {
    if (attrs.isEmpty) 
      (None, this)
    else
      attrs(uri, ns, label) match {
        case null  => (None, this)
        case contents =>
          (Some(contents), mkState(attrs.remove(uri, ns, label), nodes, ns))
      }
  }

  /**
   * Accept the given unprefixed attribute, or fail. Succeeds when the given attribute exists
   * (order does not matter). Returns a Seq[Node], since attributes may contain text nodes 
   * interspersed with entity references.
   */
  def acceptAttr(label: String): (Option[Seq[Node]], XmlInputStore) = {
    if (attrs.isEmpty)
      (None, this)
    else
      attrs(label) match {
        case null  => (None, this)
        case contents =>
          (Some(contents), mkState(attrs.remove(label), nodes, ns))
      }
  }
  
  /** Accept a text node. Fails if the head of the node list is not a text node. */
  def acceptText: (Option[Text], XmlInputStore) = {
    if (nodes.isEmpty) 
      (Some(Text("")), this)
    else 
      nodes.head match {
        case t: Text => (Some(t), mkState(attrs, nodes.tail, ns))
        case _       => (None, this)
      }
  }

  protected def mkState(attrs: MetaData, nodes: Seq[Node], ns: NamespaceBinding, level: Int) = 
    LinearStore(attrs, nodes, ns).setSkipNonElements(true)
  
  override def toString = 
    "LinearStore(" + attrs + ", " + nodes.mkString("", ",", "") + ", " + ns + ")"
  
  /** Return a text node out of the sequence of nodes (which might contain entity references). */
  private def unescapeText(ns: Seq[Node]) = {
    def unescape(sb: StringBuilder, ns: Seq[Node]): StringBuilder = ns match {
      case Seq(Text(txt), nss @ _*) =>
        sb.append(txt)
        unescape(sb, nss)

      case Seq(EntityRef(entName), nss @ _*) =>
        Utility.unescape(entName, sb)
        unescape(sb, nss)

      case Seq(a: Atom[_], nss @ _*) =>
        sb.append(a.text)
        unescape(sb, nss)

      case _ =>
        sb
    }
    unescape(new StringBuilder, ns).toString
  }
}

/**
 * Convenience object for creating LinearStores
 *
 * @author Iulian Dragos
 */
object LinearStore {
  /** Return an empty pickler state. */
  def empty: LinearStore = 
    empty(TopScope)
  
  /** Return an empty pickler state with a given namespace scope. */
  def empty(ns: NamespaceBinding) = 
    LinearStore(Null, Nil, ns)

  /** Create a LinearStore with the given state.*/
  def apply(attrs: MetaData, nodes: Seq[Node], ns: NamespaceBinding) = 
    new LinearStore(attrs, nodes.toList, ns)
  
  def apply(store: XmlStore): XmlInputStore =
    apply(store.attrs, store.nodes, store.ns)

  /** Create a LinearStore from an element. */
  def fromElem(e: Elem) =
    LinearStore(e.attributes, List(e), TopScope)
  
  /** Create a LinearStore from the given InputStream. */
  def fromInputStream(in: InputStream) = {
    val e = XML.load(in)
    fromElem(e)
  }
  
  /** Create a LinearStore from the given filename. */
  def fromFile(f: String) = {
    fromInputStream(new FileInputStream(f))
  }

  /** Create a LinearStore for the contents of the given element. */ 
  def enterElem(e: Elem) = 
    LinearStore(e.attributes, e.child.toList, e.scope)
}
