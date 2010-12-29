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


import scala.xml.{Node, Elem, MetaData, NamespaceBinding, Text, ProcInstr, 
                  Comment, TopScope, Null, XML}

/**
 * This class matches elements at any position in the sequence of nodes. This allows
 * unpicklers to accept any permutation of a defined sequence. 
 * 
 * For efficiency reasons, this class uses a mutable representation for elements. When
 * an instance is created, this class creates a map from element labels (regardless of
 * namespace) to XML elements. This allows access in constant time implementation of
 * the 'acceptElem' method.
 * 
 * @see com.google.xml.combinators.XmlInputStore
 */
class RandomAccessStore(myAttrs: MetaData, myNodes: Seq[Node], 
    myNs: NamespaceBinding, level: Int) extends LinearStore(myAttrs, myNodes.toList, myNs) {
  import collection.mutable.{Set, MultiMap}
  import collection.mutable.LinkedHashMap
  
  randomAccessLevel = level

  /** A cache of node, from element node to the Entries. */
  private val nodeMap = 
    new LinkedHashMap[String, Set[Entry]] with MultiMap[String, Entry]

  /** A holder class that provides proper identity to nodes. @see NodeBuffer.hashCode. */
  private class Entry(val n: Node)
    
  {
    // initialize store by mapping names to elements.
    for (n <- myNodes)
      nodeMap.addBinding(n.label, new Entry(n))
  }
  def this(underlying: XmlInputStore) = 
    this(underlying.attrs, underlying.nodes, underlying.ns, underlying.randomAccessLevel)
  
  /**
   * Lookup the given element, based on label and URI. It uses the node map to efficiently 
   * perform lookups and removal.
   */
  override def acceptElem(label: String, uri: String): (Option[Node], RandomAccessStore) = {
    for (elems <- nodeMap.get(label);
         val entry <- elems)
      entry.n match {
        case e: Elem if (e.namespace == uri) => 
          nodeMap.removeBinding(label, entry)
          return (Some(e), this)
      case _ => ()
    }
    (None, this)
  }
  
  /** Return the list of nodes. It reads them from the internal map. */
  override def nodes: List[Node] = {
    val buf = new scala.xml.NodeBuffer
    for (ns <- nodeMap.valuesIterator; entry <- ns.iterator) {
      buf += entry.n
    }
    buf.toList
  }
  
  /** Create a new instance of this class, given the contents of this store. */
  override protected[combinators] def mkState(attrs: MetaData, nodes: Seq[Node],
      ns: NamespaceBinding, level: Int) =
    new RandomAccessStore(attrs, nodes, ns, level)
    
  override def toString = "RandomAccessStore(" + attrs + ", " + 
    nodes.mkString("", ",", "") + ", " + ns + ")"
}

/**
 * A straight-forward implementation for random access stores. It uses immutable lists
 * and has linear element lookup. Most of the times the default RandomAccessStore is 
 * better.
 * 
 * @author Iulian Dragos

case class ListRandomAccessStore(ats: MetaData, nods: List[Node], bindings: NamespaceBinding,
    level: Int) 
    extends LinearStore(ats, nods, bindings) {

  randomAccessLevel = level
  
  override def acceptElem(label: String, uri: String): (Option[Node], ListRandomAccessStore) = {
    val elem = nodes find { n => n.isInstanceOf[Elem] && n.label == label && n.namespace == uri }
    (elem, if (elem.isEmpty) this 
        else ListRandomAccessStore(attrs, nodes.remove(_ == elem.get), ns, randomAccessLevel))
  }
  
  override protected def mkState(attrs: MetaData, nodes: Seq[Node], 
      ns: NamespaceBinding, level: Int) = ListRandomAccessStore(attrs, nodes.toList, ns, level)
    
  override def toString = "ListRandomAccessStore(" + attrs + ", " + 
    nodes.mkString("", ",", "") + ", " + ns + ")"
}
 */
