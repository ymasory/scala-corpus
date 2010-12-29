package com.google.xml.combinators

import scala.xml._
import scala.collection._

/**
 * An XML store used during pickling. It provides methods for adding
 * XML elements, attributes and namespaces. Implementers decide on the
 * actual strategy for looking up elements based on name (linear or 
 * random access).
 * 
 * @author Iulian Dragos
 */
trait XmlOutputStore extends XmlStore {
  /** Return a new XmlStore with a new attribute prepended to the list of attrs */
  def addAttribute(pre: String, key: String, value: String): XmlOutputStore
  
  /** Return a new XmlStore with an unprefixed attribute appended to the list of attrs. */
  def addAttribute(key: String, value: String): XmlOutputStore
   
  /**
   * Return a new LinearStore with a new namespace binding. If the 
   * prefix is already defined to the given URI, it returns the 
   * current object.
   */
  def addNamespace(pre: String, uri: String): XmlOutputStore

  /** Add a text node */
  def addText(s: String): XmlOutputStore =
    addNode(Text(s))

  def addNodes(ns: Seq[Node]): XmlOutputStore =
    ns.foldLeft(this) (_.addNode(_))
  
  /** Add a node. */
  def addNode(n: Node): XmlOutputStore

  /** Add an entire XmlStore to this store. */
  def addStore(other: XmlStore): XmlOutputStore

  /**
   * Return the root element of the constructed XML fragment. 
   * It always returns the first node in the list of nodes. It
   * throws an error if there are top-level attributes.
   */
  def rootNode: Elem
}

/**
 * A PlainOutputStore implements XmlOutputStore with reasonable efficiency. It
 * is a mutable representation.
 */
class PlainOutputStore(var attrs: MetaData, nods: Seq[Node], 
                       var ns: NamespaceBinding) extends XmlOutputStore {

  private val nodeBuf = new mutable.ListBuffer[Node]
  
  def nodes: Seq[Node] = nodeBuf.readOnly
  
  /** Return a new LinearStore with a prefixed attribute prepended to the list of attrs */
  def addAttribute(pre: String, key: String, value: String): XmlOutputStore = {
    attrs = new PrefixedAttribute(pre, key, value, attrs)
    this
  }

  /** Return a new LinearStore with an unprefixed attribute prepended to the list of attrs */
  def addAttribute(key: String, value: String): XmlOutputStore = {
    attrs = new UnprefixedAttribute(key, value, attrs)
    this
  }
    
  def addNamespace(pre: String, uri: String): XmlOutputStore = 
    if (ns.getURI(pre) == uri) 
      this 
    else {
      ns = new NamespaceBinding(pre, uri, ns)
      this
    }

  def addNode(n: Node) = {
    nodeBuf += n
    this
  }
  
  /**
   * Return the first element of the pickled document. Throws MalformedXmlStore if
   * the first node is not an Elem or if there are attributes not assigned to
   * an element.
   */
  def rootNode: Elem = {
    val root = nodeBuf(0)
    if (attrs.isEmpty && root.isInstanceOf[Elem]) {
      root.asInstanceOf[Elem]
    } else
      throw new MalformedXmlStore("Top-level attributes found hanging: " + attrs, this)
  }

  /** Add an entire XmlStore to this store. */
  def addStore(other: XmlStore): XmlOutputStore = {
    val newAttrs = attrs 
    other.attrs.foreach(attrs.append(_))
    nodeBuf ++= other.nodes
    this
  } 
}

/** Factory for output stores. */
object PlainOutputStore {
  /** An empty output store. */
  def empty: PlainOutputStore = new PlainOutputStore(Null, Nil, TopScope)
  
  /** An output store with a given namespace binding. */
  def apply(ns: NamespaceBinding) = new PlainOutputStore(Null, Nil, ns)
}

/** An exception thrown when the XML output store is inconsistent. */
case class MalformedXmlStore(msg: String, state: XmlOutputStore) extends RuntimeException(msg)
