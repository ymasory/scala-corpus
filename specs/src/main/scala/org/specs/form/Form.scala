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
package org.specs.form

import org.specs.matcher.{HaveTheSameElementsAs, BeEqualTo, Matcher, Matchers}
import scala.xml._
import scala.collection.mutable.ListBuffer
import org.specs.xml.NodeFunctions._
import org.specs.execute._
import org.specs.specification._
import org.specs._
import org.specs.xml.Xhtml._
import org.specs.util.ExtendedString._
import org.specs.util.Classes._
import org.specs.util.Property
/**
 * This trait defines Forms which are used to group and display Props properties together.
 *
 * The purpose of forms is to provide an easy notation to display properties associated
 * to the objects under test, to set expectations, execute and display the results as an Html table
 * expectations on them.
 *
 * @org.specs.samples.formsSpecs for an example.
 * Forms output their content in XHTML in order to be included in the output of Literate Specifications.
 * The Layout trait provides simple facilities to declare the layout of the form elements
 * inside an Html table, like aligning them on a row.
 *
 */
class Form(val titleString: Option[String], val factory: ExpectableFactory) extends DelegatedExpectableFactory(factory)
        with FormEnabled {
  /** constructor with no title, this will be set from the class name */
  def this() = this(None, new DefaultExpectableFactory {})
  /** constructor with a title */
  def this(titleString: String) = this(Some(titleString), new DefaultExpectableFactory {})
  /** constructor with a title and a specific expectable factory */
  def this(titleString: String, factory: ExpectableFactory) = this(Some(titleString), factory)
  /** constructor with no title and a specific expectable factory */
  def this(factory: ExpectableFactory) = this(None, factory)
  /** @return the title if set or build a new one based on the class name (by uncamelling it) */
  def title = titleString.getOrElse(className(this.getClass).uncamel)
  /**
   * add a subForm to this form.
   */
  override def form[F <: Form](f: F): F = {
    f.delegate = this.factory
    super.form(f)
  }
  override def copy: Form = {
    val form = new Form(label, factory)
    copyPropertiesAndFields(form)
  }
  def copyPropertiesAndFields[F <: Form](form: F) = {
    properties.foreach { (p: FormProperty with Copyable[FormProperty]) =>
      val c: FormProperty with Copyable[FormProperty] = p.copy
      c.genericFormatterIs(genericFormatter)
      form.properties.append(c)
    }
    fields.foreach { (f: Field[_]) =>
      val c = f.copy
      c.genericFormatterIs(genericFormatter)
      form.fields.append(c)
    }
    form
  }
}
/** 
 * This trait declares that an instance can return a copy of itself via the copy method.
 * It is especially used to copy EntityLineForms when testing different combinations
 * on a BagForm
 */        
trait Copyable[+T] { this : T with Copyable[T] => 
  def copy = this 
}    
trait FormEnabled extends DefaultExecutable with LabeledXhtml with Layoutable with ExpectableFactory with Copyable[FormEnabled] 
 with GenericFormatter { outer =>
  /** @return the title if set or build a new one based on the class name (by uncamelling it) */
  def title: String
  /** implementation of the HasLabel trait */
  lazy val label = title
  /** alias for properties or forms held by this Form */
  type FormProperty = DefaultExecutable with LabeledXhtml with GenericFormatter
  /** Props or Forms held by this Form */
  val properties: ListBuffer[FormProperty with Copyable[FormProperty]] = new ListBuffer
  /** Fields held by this Form */
  val fields: ListBuffer[Field[_] with Copyable[Field[_]]] = new ListBuffer
  /** alias for genericFormatterIs */
  def formatterIs(f: (String =>  String)): this.type = {
    this.genericFormatterIs(f)
  }
  override def genericFormatterIs(f: (String =>  String)) = {
    super.genericFormatterIs(f)
    propertiesAndFields.foreach(_.genericFormatterIs(f))
    this
  }
  /**
   * add a Prop to the Form.
   */
  def add(p: FormProperty with Copyable[FormProperty]*): this.type = { 
    p.foreach(properties.append(_)) 
    this 
  }
  /**
   * add a Field to the Form.
   */
  def add[T](p: Field[T]): this.type = { 
    fields.append(p) 
    this 
  }
  /**
   * this allows to set properties on this Form with:
   * myForm.set { f =>
   *   f.prop1("expectedValue1")
   *   f.prop2("expectedValue2")
   * }
   */
  def set(f: this.type => Any): this.type = { f(this); this }

  /** default execution function with a matcher */
  def executor[T]: Function2[T, Matcher[T], org.specs.specification.Result[T]] = (a: T, m: Matcher[T]) => (a must m) 
  /**
   * factory method for creating a property linked to an actual value.
   * Using this method adds the property to the Form
   */
  def prop[T](label: String, actual: =>T): MatcherProp[T] = {
    val p: MatcherProp[T] = Prop(label, actual, new MatcherConstraint(Some(actual), executor[T]))
    add(p)
    p
  }
  def prop[T](actual: =>T): MatcherProp[T] = prop("", actual)
  /**
   * factory method for creating a property linked to an actual value == to the expected value
   * Using this method adds the property to the Form
   */
  def field[T](label: String, value: =>T): Field[T] = {
    val f = new Field(label, Property(value))
    add(f)
    f
  }
  /**
   * add a subForm to this form.
   */
  def form[F <: Form](f: F): F = {
    add(f)
    f
  }
  /** implicit def allowing to write new Form {...}.formTr */
  implicit def formInRow[F <: Form](f: F) = new FormInRow(f: F)
  /** implicit def allowing to write new Form {...}.formTr */
  class FormInRow[F <: Form](f: F) {
    def formTr: F = {
      outer.form(f)
      outer.tr(f)
      f
    }
  }
  /** create a field with no label */
  def field[T](value: =>T): Field[T] = field("", value)
  /**
   * factory method for creating a field summarizing several properties values
   */
  def field[T](label: String, values: Prop[T]*) = {
    val f = Field(label, values)
    add(f)
    f
  }
  /**
   * @return implicitly the field value where this is expected
   */
  implicit def fieldToValue[T](f: Field[T]): T = f.get
  /**
   * factory method for creating an iterable property linked to an actual value.
   *
   * The default matcher for an iterable properties is "haveTheSameElementsAs"
   * Using this method adds the property to the Form.
   */
  def propIterable[T](label: String, actual: =>Iterable[T]): MatcherPropIterable[T] = {
    val matcherConstraint: MatcherConstraint[Iterable[T]] = new MatcherConstraint[Iterable[T]](Some(actual), executor)
    val p = PropIterable(label, actual, matcherConstraint)
    p.matchesWith(new HaveTheSameElementsAs(_))
    add(p)
    p
  }
  /** executing the Form is done by executing all of its properties. */
  def executeThis = {
    properties.foreach(_.execute)
    // get the rows and allow them to be layedout differently in subclasses
    val formRows = this.rows 
    resetLayout()
    layoutRows(formRows)
  }
  /** the Form failures are all the failures of the Form properties. */
  override def failures = super.failures ::: properties.toList.flatMap(_.failures)
  /** the Form skipped are all the skipped of the Form properties. */
  override def skipped = super.skipped ::: properties.toList.flatMap(_.skipped)
  /** the Form errors are all the errors of the Form properties. */
  override def errors = super.errors ::: properties.toList.flatMap(_.errors)
  /** @return a string representation of the Form with the title and one property per row. */
  override def toString = {
    title +
    properties.mkString("\n  ", "\n  ", "")
  }
  /** @return a Xhtml table representing the Form. */
  override def toXhtml = {
    spanLastTd(<table class="dataTable">
                 <tr><th>{title}</th></tr>
                 { if (!xhtml.isEmpty) xhtml else properties.map(toRow(_)) }
               </table>)
  }
  /** execute the table and return its Html as string. */
  def toHtml_! = execute.toHtml
  /** add all the properties as examples to a specification and return the html for display */
  def reportTo(s: BaseSpecification) = {

    execute
    properties.foreach { p => 
      s.forExample(title + " - " + p.label + " example") in {
        s.addExpectation
        p.issues.foreach(throw _)
      }
    }
    toHtml
  }
  /** this function can be overriden to provide a different layout of the form rows, like enclosing them in a different table or in tabs. */
  def layoutRows(formRows: List[Seq[LabeledXhtml]]) = {
    trs(formRows)
  }

  /** reset both the execution of the Form, its included/excluded properties and the layout. */
  override def reset(): this.type = {
    resetExecution()
    resetIncludeExclude()
    this
  }
  /** reset all, properties, execution, layout. */
  def resetAll(): this.type = {
    resetLayout()
    reset()
    properties.clear
    fields.clear
    this
  }

  /** reset the execution of the Form. */
  def resetExecution() = {
    properties.foreach(_.reset())
    super[DefaultExecutable].reset()
  }
  /** reset the included/excluded properties of the Form. */
  def resetIncludeExclude() = super[Layoutable].reset()
  /** @return all properties and fields for this form */
  def propertiesAndFields: List[LabeledXhtml with GenericFormatter with DefaultExecutable] = properties.toList ::: fields.toList
  /** decorate all the properties held by this form */
  override def decorateLabelsWith(x: Node => Node): this.type = { 
    propertiesAndFields.foreach(_.decorateLabelsWith(x)) 
    this 
  }
  /** decorate all the properties held by this form */
  override def decorateValuesWith(x: Node => Node): this.type = {
    propertiesAndFields.foreach(_.decorateValuesWith(x)) 
    this 
  }
  /** decorate all the properties held by this form */
  override def decorateValuesCellsWith(x: Node => Node): this.type = { 
    propertiesAndFields.foreach(_.decorateValuesCellsWith(x)) 
    this
  }
  /** decorate all the properties held by this form */
  override def decorateLabelsCellsWith(x: Node => Node): this.type = { 
    propertiesAndFields.foreach(_.decorateLabelsCellsWith(x)) 
    this
  }
}
/**
 * Some Forms can be declared as building an element of type T
 */
trait Builder[T] {
  def build: T
}
/**
 * Implicit useful conversions
 */
trait Conversions {
  implicit def stringToDouble(s: String) = java.lang.Double.parseDouble(s)
  implicit def stringToLong(s: String) = java.lang.Long.parseLong(s)
  implicit def stringToInt(s: String) = java.lang.Integer.parseInt(s)
}
