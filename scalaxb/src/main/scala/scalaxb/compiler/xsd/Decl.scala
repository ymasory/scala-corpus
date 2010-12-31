/*
 * Copyright (c) 2010 e.e d3si9n
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package scalaxb.compiler.xsd

import scala.collection.{Map, Set}
import scala.collection.mutable
import scala.collection.immutable

abstract class Decl

object Incrementor {
  var i: Int = 1000
  def nextInt: Int = {
    i = i + 1
    i
  }
}

case class XsdContext(
  schemas: mutable.ListBuffer[SchemaDecl] =
    mutable.ListBuffer.empty[SchemaDecl],
  typeNames: mutable.ListMap[Option[String],
    mutable.ListMap[Decl, String]] =
      mutable.ListMap.empty[Option[String],
      mutable.ListMap[Decl, String]],
  enumValueNames: mutable.ListMap[Option[String],
      mutable.ListMap[(String, EnumerationDecl), String]] =
      mutable.ListMap.empty[Option[String],
      mutable.ListMap[(String, EnumerationDecl), String]],
  packageNames: mutable.ListMap[Option[String], Option[String]] =
    mutable.ListMap.empty[Option[String], Option[String]],
  complexTypes: mutable.ListBuffer[(SchemaDecl, ComplexTypeDecl)] =
    mutable.ListBuffer.empty[(SchemaDecl, ComplexTypeDecl)],
  baseToSubs: mutable.ListMap[ComplexTypeDecl, List[ComplexTypeDecl]] =
    mutable.ListMap.empty[ComplexTypeDecl, List[ComplexTypeDecl]],
  compositorParents: mutable.ListMap[HasParticle, ComplexTypeDecl] =
    mutable.ListMap.empty[HasParticle, ComplexTypeDecl],
  compositorNames: mutable.ListMap[HasParticle, String] =
    mutable.ListMap.empty[HasParticle, String],
  groups: mutable.ListBuffer[(SchemaDecl, GroupDecl)] =
    mutable.ListBuffer.empty[(SchemaDecl, GroupDecl)],
  substituteGroups: mutable.ListBuffer[(Option[String], String)] =
    mutable.ListBuffer.empty[(Option[String], String)],
  prefixes: mutable.ListMap[String, String] = mutable.ListMap.empty[String, String]
    ) {
}

class ParserConfig {
  var scope: scala.xml.NamespaceBinding = _
  var targetNamespace: Option[String] = None
  val topElems  = mutable.ListMap.empty[String, ElemDecl]
  val elemList  = mutable.ListBuffer.empty[ElemDecl]
  val topTypes  = mutable.ListMap.empty[String, TypeDecl]
  val typeList  = mutable.ListBuffer.empty[TypeDecl]
  val topAttrs  = mutable.ListMap.empty[String, AttributeDecl]
  val attrList  = mutable.ListBuffer.empty[AttributeDecl]
  val topGroups = mutable.ListMap.empty[String, GroupDecl]
  val topAttrGroups = mutable.ListMap.empty[String, AttributeGroupDecl]
  val choices   = mutable.ListBuffer.empty[ChoiceDecl]
  var schemas: List[SchemaDecl] = Nil
  val typeToAnnotatable = mutable.ListMap.empty[TypeDecl, Annotatable]
}

object TypeSymbolParser {
  val XML_SCHEMA_URI = "http://www.w3.org/2001/XMLSchema"
  val XML_URI = "http://www.w3.org/XML/1998/namespace"
  
  def fromString(name: String, config: ParserConfig): XsTypeSymbol = {
    val (namespace, typeName) = splitTypeName(name, config)
    namespace match {
      case Some(XML_SCHEMA_URI) =>
        if (XsTypeSymbol.toTypeSymbol.isDefinedAt(typeName)) XsTypeSymbol.toTypeSymbol(typeName)
        else new ReferenceTypeSymbol(name)
      case _ => new ReferenceTypeSymbol(name)
    }
  }
  
  def splitTypeName(name: String, config: ParserConfig): (Option[String], String) = {
    if (name.contains('@')) (config.targetNamespace, name)
    else if (name.contains(':')) {
      val prefix = name.dropRight(name.length - name.indexOf(':'))
      val value = name.drop(name.indexOf(':') + 1)
      Option[String](config.scope.getURI(prefix)) -> value
    } else (Option[String](config.scope.getURI(null)), name)
  }
  
  def splitTypeName(name: String, schema: SchemaDecl): (Option[String], String) = {
    if (name.contains('@')) (schema.targetNamespace, name)
    else if (name.contains(':')) {
      val prefix = name.dropRight(name.length - name.indexOf(':'))
      val value = name.drop(name.indexOf(':') + 1)
      Option[String](schema.scope.getURI(prefix)) -> value
    } else (Option[String](schema.scope.getURI(null)), name)
  }  
}

trait Particle {
  val minOccurs: Int
  val maxOccurs: Int  
}

trait HasParticle extends Particle {
  val particles: List[Particle]
  val minOccurs: Int
  val maxOccurs: Int
}

trait Annotatable {
  val annotation: Option[AnnotationDecl]
}

case class SchemaDecl(targetNamespace: Option[String],
    topElems: Map[String, ElemDecl],
    elemList: List[ElemDecl],
    topTypes: Map[String, TypeDecl],
    typeList: List[TypeDecl],
    choices: List[ChoiceDecl],
    topAttrs: Map[String, AttributeDecl],
    attrList: List[AttributeDecl],
    topGroups: Map[String, GroupDecl],
    topAttrGroups: Map[String, AttributeGroupDecl],
    typeToAnnotatable: Map[TypeDecl, Annotatable],
    annotation: Option[AnnotationDecl],
    scope: scala.xml.NamespaceBinding) extends Decl with Annotatable {
  
  val newline = System.getProperty("line.separator")
  
  override def toString: String = {
    "SchemaDecl(" + newline +
    "topElems(" + topElems.valuesIterator.mkString("," + newline) + ")," + newline +
    "topTypes(" + topTypes.valuesIterator.mkString("," + newline)  + ")," + newline + 
    "topAttrs(" + topAttrs.valuesIterator.mkString("," + newline)  + ")," + newline + 
    "topGroups(" + topGroups.valuesIterator.mkString("," + newline)  + ")," + newline + 
    "topAttrGroups(" + topAttrGroups.valuesIterator.mkString("," + newline)  + ")" + newline + 
    ")"
  }
}

object SchemaDecl {
  def fromXML(node: scala.xml.Node,
      context: XsdContext = XsdContext(),
      config: ParserConfig = new ParserConfig) = {
    val schema = (node \\ "schema").headOption.getOrElse {
      error("xsd: schema element not found: " + node.toString) }
    
    config.scope = schema.scope
    config.targetNamespace = schema.attribute("targetNamespace").headOption map { _.text }
    config.schemas = context.schemas.toList
    
    for (child <- schema.child) child match {
      case <element>{ _* }</element>  =>
        (child \ "@name").headOption foreach {  x =>
          val elem = ElemDecl.fromXML(child, config.targetNamespace, config)
          config.topElems += (elem.name -> elem) }
      
      case <attribute>{ _* }</attribute>  =>
        (child \ "@name").headOption foreach {  x =>
          val attr = AttributeDecl.fromXML(child, config, true)
          config.topAttrs += (attr.name -> attr) }
            
      case <attributeGroup>{ _* }</attributeGroup>  =>
        (child \ "@name").headOption foreach {  x =>
          val attrGroup = AttributeGroupDecl.fromXML(child, config)
          config.topAttrGroups += (attrGroup.name -> attrGroup) }
      
      case <group>{ _* }</group>  =>
        (child \ "@name").headOption foreach {  x =>
          val group = GroupDecl.fromXML(child, config)
          config.topGroups += (group.name -> group) }
      
      case <complexType>{ _* }</complexType>  =>
        (child \ "@name").headOption foreach {  x =>
          val decl = ComplexTypeDecl.fromXML(child, x.text, x.text, config)
          config.typeList += decl
          config.topTypes += (decl.name -> decl) }
      
      case <simpleType>{ _* }</simpleType>  =>
        (child \ "@name").headOption foreach {  x =>
          val decl = SimpleTypeDecl.fromXML(child, x.text, x.text, config)
          config.typeList += decl
          config.topTypes += (decl.name -> decl) }
      
      case _ =>
    }
    
    var scope = schema.scope
    while (scope != null) {
      if (scope.prefix != null && scope.uri != null &&
          context.prefixes.get(scope.uri).isEmpty) context.prefixes(scope.uri) = scope.prefix
      scope = scope.parent
    } // while
                
    val annotation = (node \ "annotation").headOption map { x =>
      AnnotationDecl.fromXML(x, config) }
      
    SchemaDecl(config.targetNamespace,
      immutable.ListMap.empty[String, ElemDecl] ++ config.topElems,
      config.elemList.toList,
      immutable.ListMap.empty[String, TypeDecl] ++ config.topTypes,
      config.typeList.toList,
      config.choices.toList,
      immutable.ListMap.empty[String, AttributeDecl] ++ config.topAttrs,
      config.attrList.toList,
      immutable.ListMap.empty[String, GroupDecl] ++ config.topGroups,
      immutable.ListMap.empty[String, AttributeGroupDecl] ++ config.topAttrGroups,
      immutable.ListMap.empty[TypeDecl, Annotatable] ++ config.typeToAnnotatable,
      annotation,
      schema.scope)
  }
}

abstract class AttributeLike extends Decl

object AttributeLike {
  def fromParentNode(parent: scala.xml.Node, config: ParserConfig): List[AttributeLike] =
    for (child <- parent.child.toList;
        if child.isInstanceOf[scala.xml.Elem];
        if List("attribute", "anyAttribute", "attributeGroup").contains(
          child.label))
      yield fromXML(child, config)
  
  def fromXML(node: scala.xml.Node, config: ParserConfig): AttributeLike  = {
    if (node.label == "anyAttribute")
      AnyAttributeDecl.fromXML(node, config)
    else if (node.label == "attributeGroup")
      (node \ "@ref").headOption match {
        case Some(x) => AttributeGroupRef.fromXML(node, config)
        case None => AttributeGroupDecl.fromXML(node, config)
      }  
    else (node \ "@ref").headOption match {
      case Some(x) => AttributeRef.fromXML(node, config)
      case None => AttributeDecl.fromXML(node, config, false)
    }
  }
}

abstract class ProcessContents
object LaxProcess extends ProcessContents
object SkipProcess extends ProcessContents
object StrictProcess extends ProcessContents

case class AnyAttributeDecl(namespaceRange: String,
  processContents: ProcessContents) extends AttributeLike

object AnyAttributeDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val namespaceRange = (node \ "namespace").headOption match {
      case None    => "##any"
      case Some(x) => x.text
    }
    val processContents = (node \ "@processContents").text match {
      case "lax"  => LaxProcess
      case "skip" => SkipProcess
      case _      => StrictProcess
    }
    AnyAttributeDecl(namespaceRange, processContents)
  }
}

abstract class AttributeUse
object OptionalUse extends AttributeUse
object ProhibitedUse extends AttributeUse
object RequiredUse extends AttributeUse

case class AttributeRef(namespace: Option[String],
  name: String,
  defaultValue: Option[String],
  fixedValue: Option[String],
  use: AttributeUse) extends AttributeLike

object AttributeRef {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val ref = (node \ "@ref").text
    val (namespace, typeName) = TypeSymbolParser.splitTypeName(ref, config)
    val defaultValue = (node \ "@default").headOption map { _.text }
    val fixedValue = (node \ "@fixed").headOption map { _.text }
    val use = (node \ "@use").text match {
      case "prohibited" => ProhibitedUse
      case "required"   => RequiredUse
      case _            => OptionalUse
    }
    AttributeRef(namespace, typeName, defaultValue, fixedValue, use)
  }
}

case class AttributeDecl(namespace: Option[String],
    name: String,
    typeSymbol: XsTypeSymbol,
    defaultValue: Option[String],
    fixedValue: Option[String],
    use: AttributeUse,
    annotation: Option[AnnotationDecl],
    global: Boolean) extends AttributeLike with Annotatable {
  override def toString = "@" + name
}

object AttributeDecl {
  def fromXML(node: scala.xml.Node,
      config: ParserConfig, global: Boolean) = {
    val name = (node \ "@name").text
    var typeSymbol: XsTypeSymbol = XsUnknown
    val typeName = (node \ "@type").text
    
    if (typeName != "") {
      typeSymbol = TypeSymbolParser.fromString(typeName, config)
    } else {
      for (child <- node.child) child match {
        case <simpleType>{ _* }</simpleType> =>
          val decl = SimpleTypeDecl.fromXML(child, name, config)
          config.typeList += decl
          val symbol = new ReferenceTypeSymbol(decl.name)
          symbol.decl = decl
          typeSymbol = symbol
          
        case _ =>
      }
    } // if-else

    val defaultValue = (node \ "@default").headOption map { _.text }
    val fixedValue = (node \ "@fixed").headOption map { _.text }
    val use = (node \ "@use").text match {
      case "prohibited" => ProhibitedUse
      case "required"   => RequiredUse
      case _            => OptionalUse
    }
    val annotation = (node \ "annotation").headOption map { x =>
      AnnotationDecl.fromXML(x, config) }

    val attr = AttributeDecl(config.targetNamespace,
      name, typeSymbol, defaultValue, fixedValue, use, annotation,
      global)
    config.attrList += attr
    attr
  } 
}

case class AttributeGroupRef(namespace: Option[String],
  name: String) extends AttributeLike

object AttributeGroupRef {
  def fromXML(node: scala.xml.Node,
      config: ParserConfig) = {
    val ref = (node \ "@ref").text
    val (namespace, typeName) = TypeSymbolParser.splitTypeName(ref, config)
    
    AttributeGroupRef(namespace, typeName)
  }    
}

case class AttributeGroupDecl(namespace: Option[String],
  name: String,
  attributes: List[AttributeLike],
  annotation: Option[AnnotationDecl]) extends AttributeLike with Annotatable
  
object AttributeGroupDecl {
  def fromXML(node: scala.xml.Node,
      config: ParserConfig) = {
    val name = (node \ "@name").text
    val attributes = AttributeLike.fromParentNode(node, config)
    val annotation = (node \ "annotation").headOption map { x =>
      AnnotationDecl.fromXML(x, config) }
    
    AttributeGroupDecl(config.targetNamespace,
      name, attributes.toList.reverse, annotation)
  } 
}

case class ElemRef(namespace: Option[String],
  name: String,
  minOccurs: Int,
  maxOccurs: Int,
  nillable: Option[Boolean]) extends Decl with Particle

object ElemRef {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val ref = (node \ "@ref").text   
    val minOccurs = CompositorDecl.buildOccurrence((node \ "@minOccurs").text)
    val maxOccurs = CompositorDecl.buildOccurrence((node \ "@maxOccurs").text)
    val (namespace, typeName) = TypeSymbolParser.splitTypeName(ref, config)
    val nillable = (node \ "@nillable").headOption map { _.text == "true" }
    
    ElemRef(namespace, typeName, minOccurs, maxOccurs, nillable)
  }
}

case class ElemDecl(namespace: Option[String],
  name: String,
  typeSymbol: XsTypeSymbol,
  defaultValue: Option[String],
  fixedValue: Option[String],  
  minOccurs: Int,
  maxOccurs: Int,
  nillable: Option[Boolean],
  substitutionGroup: Option[(Option[String], String)],
  annotation: Option[AnnotationDecl]) extends Decl with Particle with Annotatable

object ElemDecl {
  def fromXML(node: scala.xml.Node, namespace: Option[String], config: ParserConfig) = {
    val name = (node \ "@name").text
    var typeSymbol: XsTypeSymbol = XsAny
    val typeName = (node \ "@type").text
    
    if (typeName != "") {
      typeSymbol = TypeSymbolParser.fromString(typeName, config)
    } else {
      for (child <- node.child) child match {
        case <complexType>{ _* }</complexType> =>
          val decl = ComplexTypeDecl.fromXML(child, "complexType@" + name, name, config)
          config.typeList += decl
          val symbol = new ReferenceTypeSymbol(decl.name)
          symbol.decl = decl
          typeSymbol = symbol
          
        case <simpleType>{ _* }</simpleType> =>
          val decl = SimpleTypeDecl.fromXML(child, name, config)
          config.typeList += decl
          val symbol = new ReferenceTypeSymbol(decl.name)
          symbol.decl = decl
          typeSymbol = symbol
        
        case _ =>
      }
    } // if-else
    
    val defaultValue = (node \ "@default").headOption map { _.text }
    val fixedValue = (node \ "@fixed").headOption map { _.text }
    val minOccurs = CompositorDecl.buildOccurrence((node \ "@minOccurs").text)
    val maxOccurs = CompositorDecl.buildOccurrence((node \ "@maxOccurs").text)
    val nillable = (node \ "@nillable").headOption map { _.text == "true" }
    val substitutionGroup = (node \ "@substitutionGroup").headOption map { x =>
      TypeSymbolParser.splitTypeName(x.text, config) }
    val annotation = (node \ "annotation").headOption map { x =>
      AnnotationDecl.fromXML(x, config) }
    
    val elem = ElemDecl(namespace, 
      name, typeSymbol, defaultValue, fixedValue, minOccurs, maxOccurs, nillable,
      substitutionGroup, annotation)
    config.elemList += elem
    if (typeName == "") typeSymbol match {
      case ReferenceTypeSymbol(decl: ComplexTypeDecl) =>
        config.typeToAnnotatable += (decl -> elem)
      case ReferenceTypeSymbol(decl: SimpleTypeDecl) =>
        config.typeToAnnotatable += (decl -> elem)
      case _ =>
    }
    
    elem
  }
}


trait TypeDecl extends Decl with Annotatable

/** simple types cannot have element children or attributes.
 */
case class SimpleTypeDecl(namespace: Option[String],
    name: String,
    family: String, // same as name if it's named, or the inner most named node like element.
    content: ContentTypeDecl,
    annotation: Option[AnnotationDecl]) extends TypeDecl {
  override def toString(): String = name + "(" + content.toString + ")"
  def isNamed = (name == family)
}

object SimpleTypeDecl {
  def fromXML(node: scala.xml.Node, family: String, config: ParserConfig): SimpleTypeDecl =
    fromXML(node, "simpleType@" + family + ":" + Incrementor.nextInt, family, config)
  
  def fromXML(node: scala.xml.Node, name: String, family: String, config: ParserConfig): SimpleTypeDecl = {
    var content: ContentTypeDecl = null
    for (child <- node.child) child match {
      case <restriction>{ _* }</restriction>  => content = SimpTypRestrictionDecl.fromXML(child, family, config) 
      case <list>{ _* }</list>                => content = SimpTypListDecl.fromXML(child, family, config)
      case <union>{ _* }</union>              => content = SimpTypUnionDecl.fromXML(child, config)
      case _ =>     
    }
    
    val annotation = (node \ "annotation").headOption map { x =>
      AnnotationDecl.fromXML(x, config) }
    
    SimpleTypeDecl(config.targetNamespace, name, family, content, annotation)
  }
}

/** complex types may have element children and attributes.
 */
case class ComplexTypeDecl(namespace: Option[String],
    name: String,
    family: String, // same as name if it's named, or the inner most named node like element.
    abstractValue: Boolean,
    mixed: Boolean,
    content: HasComplexTypeContent,
    attributes: List[AttributeLike],
    annotation: Option[AnnotationDecl]) extends TypeDecl {
  def isNamed = (name == family)    
}

object ComplexTypeDecl {  
  def fromXML(node: scala.xml.Node, name: String, family: String, config: ParserConfig) = {
    val abstractValue = (node \ "@abstract").headOption match {
      case Some(x) => x.text.toBoolean
      case None    => false
    }
    
    val mixed = (node \ "@mixed").headOption match {
      case Some(x) => x.text.toBoolean
      case None    => false
    }
    
    val attributes = AttributeLike.fromParentNode(node, config)
    var content: HasComplexTypeContent = ComplexContentDecl.fromAttributes(attributes)
    
    for (child <- node.child) child match {
      case <group>{ _* }</group> =>
        content = ComplexContentDecl.fromCompositor(
          CompositorDecl.fromXML(child, config), attributes)
      case <all>{ _* }</all> =>
        content = ComplexContentDecl.fromCompositor(
          CompositorDecl.fromXML(child, config), attributes)
      case <choice>{ _* }</choice> =>
        content = ComplexContentDecl.fromCompositor(
          CompositorDecl.fromXML(child, config), attributes)
      case <sequence>{ _* }</sequence> =>
        content = ComplexContentDecl.fromCompositor(
          CompositorDecl.fromXML(child, config), attributes)
      case <simpleContent>{ _* }</simpleContent> =>
        content = SimpleContentDecl.fromXML(child, family, config)
      case <complexContent>{ _* }</complexContent> =>
        content = ComplexContentDecl.fromXML(child, config)
      case _ =>
    }
    
    val annotation = (node \ "annotation").headOption map { x =>
      AnnotationDecl.fromXML(x, config) }
    
    // val contentModel = ContentModel.fromSchema(firstChild(node))
    ComplexTypeDecl(config.targetNamespace, name, family, abstractValue, mixed, 
      content, attributes.reverse, annotation)
  }
}

trait HasComplexTypeContent {
  val content: ComplexTypeContent
}

trait HasContent {
  val content: ContentTypeDecl
}

/** complex types with simple content only allow character content.
 */
case class SimpleContentDecl(content: ComplexTypeContent) extends Decl with HasComplexTypeContent

object SimpleContentDecl {
  def fromXML(node: scala.xml.Node, family: String, config: ParserConfig) = {
    var content: ComplexTypeContent = null
    
    for (child <- node.child) child match {
      case <restriction>{ _* }</restriction> =>
        content = SimpContRestrictionDecl.fromXML(child, family, config)
      case <extension>{ _* }</extension> =>
        content = SimpContExtensionDecl.fromXML(child, family, config)
      case _ =>
    }
       
    SimpleContentDecl(content)
  }
}

/** only complex types with complex content allow child elements
 */
case class ComplexContentDecl(content: ComplexTypeContent) extends Decl with HasComplexTypeContent

object ComplexContentDecl {
  lazy val empty =
    ComplexContentDecl(CompContRestrictionDecl.empty)
  
  def fromAttributes(attributes: List[AttributeLike]) =
    ComplexContentDecl(CompContRestrictionDecl.fromAttributes(attributes))
  
  def fromCompositor(compositor: HasParticle, attributes: List[AttributeLike]) =
    ComplexContentDecl(CompContRestrictionDecl.fromCompositor(compositor, attributes))
  
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    var content: ComplexTypeContent = CompContRestrictionDecl.empty
    
    for (child <- node.child) child match {
      case <restriction>{ _* }</restriction> =>
        content = CompContRestrictionDecl.fromXML(child, config)
      case <extension>{ _* }</extension> =>
        content = CompContExtensionDecl.fromXML(child, config)
      case _ =>
    }
    
    ComplexContentDecl(content)
  }
}

abstract class CompositorDecl extends Decl

object CompositorDecl {
  def fromNodeSeq(seq: scala.xml.NodeSeq, config: ParserConfig): List[Particle] =
    (seq.toList.collect {
      case elem: scala.xml.Elem
        if (elem.label != "annotation") &&
          (elem.label != "attribute") => elem
    }) map(node =>
      node match {
        case <element>{ _* }</element>   =>
          if ((node \ "@name").headOption.isDefined) ElemDecl.fromXML(node, None, config)
          else if ((node \ "@ref").headOption.isDefined) ElemRef.fromXML(node, config)
          else error("xsd: Unspported content type " + node.toString) 
        case <choice>{ _* }</choice>     => ChoiceDecl.fromXML(node, config)
        case <sequence>{ _* }</sequence> => SequenceDecl.fromXML(node, config)
        case <all>{ _* }</all>           => AllDecl.fromXML(node, config)
        case <any>{ _* }</any>           => AnyDecl.fromXML(node, config)
        case <group>{ _* }</group>       =>
          if ((node \ "@name").headOption.isDefined) GroupDecl.fromXML(node, config)
          else if ((node \ "@ref").headOption.isDefined) GroupRef.fromXML(node, config)
          else error("xsd: Unspported content type " + node.toString) 

        case _ => error("xsd: Unspported content type " + node.label)    
      }
    )
  
  def fromXML(node: scala.xml.Node, config: ParserConfig): HasParticle = node match {
    case <choice>{ _* }</choice>     => ChoiceDecl.fromXML(node, config)
    case <sequence>{ _* }</sequence> => SequenceDecl.fromXML(node, config)
    case <all>{ _* }</all>           => AllDecl.fromXML(node, config)
    case <group>{ _* }</group>       =>
      if ((node \ "@name").headOption.isDefined) GroupDecl.fromXML(node, config)
      else if ((node \ "@ref").headOption.isDefined) GroupRef.fromXML(node, config)
      else error("xsd: Unspported content type " + node.toString)
    
    case _ => error("xsd: Unspported content type " + node.label)
  }
  
  def buildOccurrence(value: String) =
    if (value == "") 1
    else if (value == "unbounded") Integer.MAX_VALUE
    else value.toInt
}

case class SequenceDecl(particles: List[Particle],
  minOccurs: Int,
  maxOccurs: Int,
  uniqueId: Int = Incrementor.nextInt) extends CompositorDecl with HasParticle

object SequenceDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val minOccurs = CompositorDecl.buildOccurrence((node \ "@minOccurs").text)
    val maxOccurs = CompositorDecl.buildOccurrence((node \ "@maxOccurs").text)
    SequenceDecl(CompositorDecl.fromNodeSeq(node.child, config), minOccurs, maxOccurs)
  }
}

case class ChoiceDecl(particles: List[Particle],
  minOccurs: Int,
  maxOccurs: Int,
  uniqueId: Int = Incrementor.nextInt) extends CompositorDecl with HasParticle

object ChoiceDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val minOccurs = CompositorDecl.buildOccurrence((node \ "@minOccurs").text)
    val maxOccurs = CompositorDecl.buildOccurrence((node \ "@maxOccurs").text)
    val choice = ChoiceDecl(CompositorDecl.fromNodeSeq(node.child, config), minOccurs, maxOccurs)
    config.choices += choice
    choice
  }
}

case class AllDecl(particles: List[Particle],
  minOccurs: Int,
  maxOccurs: Int,
  uniqueId: Int = Incrementor.nextInt) extends CompositorDecl with HasParticle

object AllDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val minOccurs = CompositorDecl.buildOccurrence((node \ "@minOccurs").text)
    val maxOccurs = CompositorDecl.buildOccurrence((node \ "@maxOccurs").text)
    AllDecl(CompositorDecl.fromNodeSeq(node.child, config), minOccurs, maxOccurs)
  }
}

case class AnyDecl(minOccurs: Int,
  maxOccurs: Int) extends CompositorDecl with Particle

object AnyDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val minOccurs = CompositorDecl.buildOccurrence((node \ "@minOccurs").text)
    val maxOccurs = CompositorDecl.buildOccurrence((node \ "@maxOccurs").text)
    AnyDecl(minOccurs, maxOccurs)
  }
}

case class GroupRef(namespace: Option[String],
  name: String,
  particles: List[Particle],
  minOccurs: Int,
  maxOccurs: Int) extends CompositorDecl with HasParticle

object GroupRef {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val ref = (node \ "@ref").text   
    val minOccurs = CompositorDecl.buildOccurrence((node \ "@minOccurs").text)
    val maxOccurs = CompositorDecl.buildOccurrence((node \ "@maxOccurs").text)
    val (namespace, typeName) = TypeSymbolParser.splitTypeName(ref, config)
    
    GroupRef(namespace, typeName, Nil, minOccurs, maxOccurs)
  }
}

case class GroupDecl(namespace: Option[String],
  name: String,
  particles: List[Particle],
  minOccurs: Int,
  maxOccurs: Int,
  annotation: Option[AnnotationDecl]) extends CompositorDecl with HasParticle with Annotatable

object GroupDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    val name = (node \ "@name").text
    val minOccurs = CompositorDecl.buildOccurrence((node \ "@minOccurs").text)
    val maxOccurs = CompositorDecl.buildOccurrence((node \ "@maxOccurs").text)
    
    val annotation = (node \ "annotation").headOption map { x =>
      AnnotationDecl.fromXML(x, config) }
    val group = GroupDecl(config.targetNamespace, name,
      CompositorDecl.fromNodeSeq(node.child, config), minOccurs, maxOccurs,
      annotation)
    // config.choices += choice
    group
  }
}

case class SchemaLite(targetNamespace: Option[String],
    imports: List[ImportDecl], includes: List[IncludeDecl])

object SchemaLite {
  def fromXML(node: scala.xml.Node) = {
    val schema = (node \\ "schema").headOption getOrElse {
      error("xsd: schema element not found: " + node.toString)
    }
    val targetNamespace = schema.attribute("targetNamespace").headOption map { _.text }
    
    var importList: List[ImportDecl] = Nil
    for (node <- schema \ "import") {
      val decl = ImportDecl.fromXML(node)
      importList = decl :: importList
    }
    
    var includeList: List[IncludeDecl] = Nil
    for (node <- schema \ "include") {
      val decl = IncludeDecl.fromXML(node)
      includeList = decl :: includeList
    }
    
    SchemaLite(targetNamespace, importList.reverse, includeList.reverse)
  }
}

case class ImportDecl(namespace: Option[String],
    schemaLocation: Option[String]) extends Decl

object ImportDecl {
  def fromXML(node: scala.xml.Node) = {
    val namespace = (node \ "@namespace").headOption map { _.text }
    val schemaLocation = (node \ "@schemaLocation").headOption map { _.text }
    ImportDecl(namespace, schemaLocation)
  }
}

case class IncludeDecl(schemaLocation: String) extends Decl

object IncludeDecl {
  def fromXML(node: scala.xml.Node) = {
    val schemaLocation = (node \ "@schemaLocation").text
    IncludeDecl(schemaLocation)
  }
}

case class AnnotationDecl(documentations: Seq[DocumentationDecl]) extends Decl

object AnnotationDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) = {
    AnnotationDecl(
      for (child <- node \ "documentation")
        yield DocumentationDecl.fromXML(child, config))
  }  
}

case class DocumentationDecl(any: Seq[Any]) extends Decl

object DocumentationDecl {
  def fromXML(node: scala.xml.Node, config: ParserConfig) =
    DocumentationDecl(node.child.collect {
      case x: scala.xml.Text => x.data
      case x => x
    })
}
