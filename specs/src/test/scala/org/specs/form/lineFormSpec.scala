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

class lineFormSpec extends org.spex.Specification {
  val lineForm = new LineForm {
        prop("First Name", "Hello")
        prop("Last Name", "World")
      }
  "a line form" should {
    "display all values when rendering its Xhtml method" in {
      lineForm.toHtml.toString must include("Hello") and include("World")
    }
    "have no labels displayed when rendering its Xhtml method" in {
      lineForm.toHtml.toString must not include("First Name")
    }
    "return a header with the properties labels" in {
      lineForm.header.toString must include("First Name") and include("Last Name")
    }
    "return a modified header with specific attributes" in {
      (lineForm % <t width="50%"/>.attributes).header.toString must include("width=\"50%\"")
    }
    "return a row containing the properties values instead when queried for rows" in {
      lineForm.rows must have size 1
      lineForm.rows aka "multiple rows query" must have size 1
    }
  }
  "An entity line form" should {
    "reset its actual values when the entity changes" in {
      val stringSize = new EntityLineForm[String] {
        prop((_:String).size)(5)
      }
      val update = (a: Option[String]) => stringSize.entityIs(a)
      update(None).execute.isOk must be(true)
      update(Some("Hello")).execute.isOk must be(true)
      update(Some("My dear")).execute.isOk must be(false)
    }
    "have a testWith method returning a copy of the form with a specific entity" in {
      val stringSize = new EntityLineForm[String] {
        val p = prop((_:String).size)(5)
      }
      stringSize.entityIs("Helloooooo")
      val stringSize2 = stringSize.testWith("World")
      stringSize must !=(stringSize2)

      stringSize2.entity.isDefined must beTrue
      stringSize2.entity.get must_== "World"
      stringSize2.execute.isOk must beTrue
      stringSize2.rows aka "table rows" must not be empty
      stringSize2.rows(0) aka "table first row" must not be empty
    }
    "have a toEmbeddedXhtml method returning the properties on the same row without labels" in {
      val form = new EntityLineForm[String]{
        val p = prop((_:String).size)(5)
        val p2 = prop((_:String).size)(5)
        tr(p, p2)
      }
      form.toEmbeddedXhtml must ==/(scala.xml.Group(<td class="info">5</td><td class="info">5</td>))
    }
    "decorate all fields and properties when decorated" in {
      val form = new EntityLineForm[String]{
        val p = prop((_:String).size)(5)
      }.italic
      form.p.toXhtml must \\("i")
    }

  }
}
