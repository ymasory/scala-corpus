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
import scala.xml._
/**
 * This class represent properties which can be updated and retrieved using customized getter and setter functions.
 * 
 * The held value is optional: it may not exist yet and it is lazy: it is evaluated only once on the first call to get
 */
class Property[T](var value: () => Option[T]) {
  /** true when the value will have been evaluated once */
  private var valueHasBeenEvaluated = false
  /** first evaluation of the Property's value */
  private var evaluatedValue: Option[T] = None
  /**
   * setter function used to set the property value. The default is the identity function
   */
  private var setter: T => T = identity[T]

  /**
   * getter function used to retrieve the property value. The default is the identity function
   */
  private var getter: T => T = identity[T]

  /**
   * display function used to print the property value. The default is the toString method
   */
  private var toStringer: T => String = (v: T) => v.toString

  /** change the value */
  def withValue(init: =>T): this.type = updateValue(Some(init))
  /** change the value */
  def updateValue(init: =>Option[T]): this.type = {
    value = () => init
    valueHasBeenEvaluated = false
    this
  }
  /** @return the value as an Option */
  def optionalValue = {
    if (!valueHasBeenEvaluated) {
      evaluatedValue = value()
      valueHasBeenEvaluated = true
    }
    evaluatedValue
  }
  /**
   * @returns a value using the getter function
   */
  def get: T = getter(optionalValue.get)
  /**
   * @returns a value using the getter function
   */
  def apply(): T = get
  def apply(newValue: =>T): this.type = update(newValue)
  /**
   * updates the value using the setter function
   */
  def update(newValue: =>T): this.type = withValue(newValue)
  /**
   * updates the value using the setter function, forcing the new value to be evaluated.
   * This avoids stack overflow errors when doing property(property() + 1) for example
   */
  def forceUpdate(newValue: T): this.type = withValue(newValue)
  /**
   * updates the value with a mapping function
   */
  def update(f: T => T): this.type = this.value() match {
	  case Some(v) => forceUpdate(f(v))
	  case _ => this
  }
  /**
   * sets a new getter function
   */
  def onGet(newGetter: T => T): this.type = { getter = newGetter; this }

  /**
   * sets a new setter function
   */
  def onSet(newSetter: T => T): this.type = { setter = newSetter; this }

  /**
   * sets a new display function
   */
  def onToString(newToStringer: T => String): this.type = { toStringer = newToStringer; this }

  /**
   * @returns the string value using the stringer function
   */
  override def toString = toStringer(get)
  
  /** @return an iterator containing the value if present */
  def iterator = optionalValue.iterator
  /** return the property with the value being filtered according to a predicate */
  def filter(p: T => Boolean): this.type = { 
    val v = value() 
    value = () => v.filter(p)
    this 
  }
  /** option-like flatMap */
  def flatMap[U](f: T => Option[U]): Property[U] = new Property(() => optionalValue.flatMap(f))
  /** option-like foreach */
  def foreach(f: T => Unit): Unit = optionalValue.foreach(f)
  /** option-like getOrElse */
  def getOrElse[U >: T](other: U): U = optionalValue.getOrElse(other)
  /** option-like isDefined */
  def isDefined = optionalValue.isDefined
  /** option-like isEmpty */
  def isEmpty = optionalValue.isEmpty
  /** option-like map */
  def map[U](f: T => U): Property[U] = new Property(() => optionalValue.map(f(_)))
  /** option-like orElse */
  def orElse[U >: T](other: => Property[U]): Property[U] = new Property(() => optionalValue.orElse(other.optionalValue))
  /** option-like toLeft */
  def toLeft[R](right: R) = optionalValue.toLeft(right)
  /** option-like toRight */
  def toRight[L](left: L) = optionalValue.toRight(left)
  /** to a list */
  def toList = optionalValue.toList
  
  
  override def equals(other: Any) = {
    other match {
      case o: Property[_] => o.optionalValue == optionalValue
      case _ => false
    }
  }
}
/**
 * Companion object to create properties with possibly no initial value
 */
object Property {
  def apply[T](i: =>T) = new Property(() => Some(i))
  def apply[T]() = new Property[T](() => None)
}
/**
 * This class is a Property which can be reinitialized with its first value
 * 
 */
class ReinitProperty[T](var initValue: () => Option[T]) extends Property(initValue) {
  /** @return the Property with its original initial value */
  def reinit: this.type = { initValue().map(v => this.update(v)); this }
}
/**
 * Companion object to create reinitializable properties with possibly no initial value
 */
object ReinitProperty {
  def apply[T](i: =>T) = new ReinitProperty(() => Some(i))
  def apply[T]() = new ReinitProperty[T](() => None)
}

object Properties extends Properties
trait Properties {
    /**
   * This method is used setup a property value, in order to avoid repeting a string. For example: <pre>
   * The name of the person should be {"john" as personName in checkPersonName}
   * </pre>. 
   */
  implicit def anyToAs[T](a: T) = new AsProperty(a)
  implicit def propertyToValue[T](p: Property[T]):T = p()
  case class AsProperty[T](a: T) { 
    def as(p: Property[T]) = {p() = a; a }
    def apply(p: Property[T]) = {p() = a; a}
    def apply(f: T => Any)= {f(a); a }
    def as(f: T => Any)= {f(a); a }
  }

}
/**
 * This trait provides String properterties with alphabetical names.
 */
trait StringProperties { outer =>
  val a = Property[String]("")
  val b = Property[String]("")
  val c = Property[String]("")
  val d = Property[String]("")
  val e = Property[String]("")
  val f = Property[String]("")
  implicit def stringToAlpha(value: String) = StringToAlpha(value)
  case class StringToAlpha(value: String) {
    def a = { outer.a() = value; value }
    def b = { outer.b() = value; value }
    def c = { outer.c() = value; value }
    def d = { outer.d() = value; value }
    def e = { outer.e() = value; value }
    def f = { outer.f() = value; value }
  } 
}
/**
 * This trait provides Xml properterties with alphabetical names.
 */
trait XmlProperties { outer =>
  val xml  = Property[Elem](<e/>)
  val xml2 = Property[Elem](<e/>)
  val xml3 = Property[Elem](<e/>)
  val xml4 = Property[Elem](<e/>)
  val xml5 = Property[Elem](<e/>)
  val xml6 = Property[Elem](<e/>)
  implicit def elemToAlpha(value: Elem) = ElemToAlpha(value)
  case class ElemToAlpha(value: Elem) {
    def xml  = { outer.xml() = value; value }
    def xml2 = { outer.xml2() = value; value }
    def xml3 = { outer.xml3() = value; value }
    def xml4 = { outer.xml4() = value; value }
    def xml5 = { outer.xml5() = value; value }
    def xml6 = { outer.xml6() = value; value }
  } 
}
/**
 * This trait provides Int properterties with alphabetical names.
 */
trait IntProperties { outer =>
  val i = Property[Int](0)
  val j = Property[Int](0)
  val k = Property[Int](0)
  val l = Property[Int](0)
  val m = Property[Int](0)
  val n = Property[Int](0)
  implicit def intToAlpha(value: Int) = IntToAlpha(value)
  case class IntToAlpha(value: Int) {
    def i = { outer.i(value); value }
    def j = { outer.j(value); value }
    def k = { outer.k(value); value }
    def l = { outer.l(value); value }
    def m = { outer.m(value); value }
    def n = { outer.n(value); value }
  } 
}
/**
 * This trait provides Boolean properterties with alphabetical names.
 */
trait BooleanProperties { outer =>
  val o = Property(true)
  val p = Property(true)
  val q = Property[Boolean](true)
  val r = Property[Boolean](true)
  val s = Property[Boolean](true)
  val t = Property[Boolean](true)
  implicit def booleanToAlpha(value: Boolean) = BooleanToAlpha(value)
  case class BooleanToAlpha(value: Boolean) {
    def o = { outer.o() = value; value }
    def p = { outer.p() = value; value }
    def q = { outer.q() = value; value }
    def r = { outer.r() = value; value }
    def s = { outer.s() = value; value }
    def t = { outer.t() = value; value }
  } 
}
/**
 * This trait provides Double properterties with alphabetical names.
 */
trait DoubleProperties { outer =>
  val u = Property[Double](0.0)
  val v = Property[Double](0.0)
  val w = Property[Double](0.0)
  val x = Property[Double](0.0)
  val y = Property[Double](0.0)
  val z = Property[Double](0.0)
  implicit def doubleToAlpha(value: Double) = DoubleToAlpha(value)
  case class DoubleToAlpha(value: Double) {
    def u = { outer.u(value); value }
    def v = { outer.v(value); value }
    def w = { outer.w(value); value }
    def x = { outer.x(value); value }
    def y = { outer.w(value); value }
    def z = { outer.z(value); value }
  } 
}
/**
 * This trait sets one String property for a current value.
 */
trait CurrentProperty { outer => 
  val it = Property[String]("")
  implicit def stringToIt(s: String) = StringToIt(s)
  case class StringToIt(s: String) {
    def it = { outer.it() = s; s }
  } 
  
}
/**
 * This trait adds all properties.
 */
trait AllProperties extends StringProperties with IntProperties with DoubleProperties with BooleanProperties with XmlProperties with CurrentProperty 
object AllProperties extends AllProperties
