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
import org.specs.util.Property
import org.specs.matcher.Matcher
import scala.collection.mutable._
import scala.xml._

class EntityLineForm[T] extends LineForm {
  var entity: Property[T] = Property[T]()
  val entityProperties = new ListBuffer[EntityLineProp[T, _]]
  /** add a new LineProp to that line */
  def prop[S](s: String, f:(T => S)): LineProp[S] = {
    val p: EntityLineProp[T, S] = new EntityLineProp[T, S](label, Property[S](), f, entity, Some(new MatcherConstraint(entity.map(f(_)).optionalValue, executor)))
    entityProperties.append(p)
    lineProperties.append(p)
    properties.append(p)
    p
  }
  /** add a new LineProp to that line */
  def prop[S](f:(T => S)): LineProp[S] = prop("", f) 
  /** in that case a LineField is modeled as a commented line prop */
  def field[S](s: String, f:(T => S)): LineProp[S] = prop(s, f).comment
  /** in that case a LineField is modeled as a commented line prop */
  def field[S](f:(T => S)): LineProp[S] = field("", f) 
  def entityIs(a: T): this.type = entityIs(Some(a))
  def entityIs(a: Option[T]): this.type = { 
    a.map(entity(_))
    a.map(e => entityProperties.map(_.asInstanceOf[EntityLineProp[T, _]]entity(e)))
    this
  }
  def testWith(a: T): EntityLineForm[T] = testWith(Some(a))
  def testWith(a: Option[T]): EntityLineForm[T] = {
    val c = copy
    a.map(c.entityIs(_))
    a.map(e => c.entityProperties.map(_.asInstanceOf[EntityLineProp[T, _]].entity(e)))
    c
  }
   
  override def resetAll(): this.type = {
    super.resetAll
    entityProperties.clear()
    this
  }

  override def copy: EntityLineForm[T] = {
    val form = new EntityLineForm[T]
    this.lineProperties.foreach(p => form.lineProperties.append(p.copy))
    this.properties.foreach(p => form.properties.append(p.copy))
    this.fields.foreach(f => form.fields.append(f.copy))
    super.copy(form)
    form
  }
}
