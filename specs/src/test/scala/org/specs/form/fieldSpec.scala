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
import Field._
import org.specs.util.Property

class fieldSpec extends org.spex.Specification {
  "A Field" should {
    "update its value with the apply method and get it with the get method" in {
      Field("label", 1)(2).get must_== 2
    }
    "not evaluate its value until it is explicitly required" in {
      var evaluated = false
      val field = Field("label", { evaluated = true; 1 })
      evaluated must be(false)
      field.get
      evaluated must be(true)
    }
    "use the apply() method as an alias for get to get the Field value" in {
      Field("label", 1)() must_== 1
    }
    "have a toString method displaying the label and value" in {
      Field("label", "value").toString must_== "label: value"
    }
    "have a toString method formatting Doubles using the US format by default" in {
      Field("label", 1.2345).toString must_== "label: 1.2345"
    }
    "have a toXhtml method returning the formatted value in a <td> cell" in {
      Field("label", 1.2345).toXhtml.toList must ==(<td>label</td><td class="info">1.2345</td>.toList) 
    }
    "have a toXhtml method omitting the label if it is empty" in {
      Field(1.2345).toXhtml.toList must ==(<td class="info">1.2345</td>.toList) 
    }
    "have an equals method comparing labels and values" in {
      Field("l", 1.23) must_== Field("l", 1.23)
      Field("n", 1.23) must_!= Field("l", 1.23)
      Field("l", 1.23) must_!= Field("l", 1.22)
    }
    "have a toStringField method converting a Field[T] to a Field[String] using the value toString method" in {
      Field(1.23).toStringField must ==(Field("1.23"))
    }
  }
  "A Field" can {
    "set a success style attribute to decorate the value" in {
      val f = Field("Result", 1.12).successValue.toXhtml(1)
      f must ==/(<td class="success">1.12</td>)
    }
    "set both a success style attribute and a bold style to decorate the value" in {
      val f = Field("Result", 1.12).boldValue.successValue.toXhtml(1)
      f must ==/(<td class="success"><b>1.12</b></td>)
    }
    "set an attribute, like bgcolor, on a value cell" in {
      val f = Field("Result", 1.12).valueCellAttribute("bgcolor", "#FF6699").toXhtml(1)
      f must ==/(<td class="info" bgcolor="#FF6699">1.12</td>)
    }
    "set the bg color of a cell, setting the class attribute to 'none'" in {
      val f = Field("Result", 1.12).valueBgcolor("#FF6699").toXhtml(1)
	  f must ==/(<td bgcolor="#FF6699" class="none">1.12</td>)
    }
  }
  "The Field object" can {
    "concanate other fields" in {
      val f1 = Field("f1", "value1")
      val f2 = Field("f2", "value2")

      Field("label", f1, f2).toString must_== "label: value1/value2"
    }
    "concanate other fields with a custom separator" in {
      val f1 = Field("f1", "value1")
      val f2 = Field("f2", "value2")

      Field("label", ", ", f1, f2).toString must_== "label: value1, value2"
    }
    "copy the value formatter and decorators of the first field to the other fields" in {
      val f1 = Field("f1", "value1")
      f1.bold
      val f2 = Field("f2", "value2")

      val f3 = Field("label", f1, f2)
      f3.toXhtml must \\("b")
    }
  }
}
