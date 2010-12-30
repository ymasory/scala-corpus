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
import org.specs._
import org.specs.execute._
import org.specs.matcher._
import org.specs.runner._
import org.specs.mock.Mockito
import org.specs.specification._
import org.specs.util._
import scala.xml._

class propSpec extends SpecificationWithJUnit with Mockito with Sugar with DataTables {
  "A property" should {
    "return the expected value with the get method" in {
      Prop("label", 1)(2).get must ==(2)
    }
  }
  "A property with a constraint" should {
    
    (("block", mock[AnyConstraint[Int]]), 
     ("function", mock[FunctionConstraint[Int]]), 
     ("matcher", mock[MatcherConstraint[Int]])) foreach { t => val (label, constraint) = t 
                                                          
      "evaluate the constraint when a "+label+" property is executed" in {  
        Prop(label, 1, constraint)(2).execute
        there was one(constraint).execute(Some(2))
      }
      
    }
  }
  "A property toString method" should {
    "return an underscore if the actual value is not set" in {
      Prop("Name")("Eric").toString must_== "Name: _ (expected: Eric)"
    }
    "return an underscore if the expected value is not set" in {
      Prop("Name", "Eric").toString must_== "Name: Eric (expected: _)"
    }
    "return the actual and expected values if they are set" in {
      Prop("Name", "Eric")("Max").toString must_== "Name: Eric (expected: Max)"
    }
    "return a properly formatted Double value" in {
      Prop("label", 1.2345).toString must_== "label: 1.2345 (expected: _)"
    }
  }
  "A property toXhtml method" should {
    "display the actual value and not display the label if it is empty" in {
      Prop("", 1).toXhtml must ==/(<td class="info">1</td>)
    }
    "display the label and the actual value if the label is not empty" in {
      Prop("label", 1).toXhtml must ==/(<td>label</td><td class="info">1</td>)
    }
    "set the class attribute of the value as failure if the Prop has failed" in {
      (Prop("Result", 1, {fail(""); 1}).toXhtml_! \\("@class")).toString must_== "failure"
    }
    "set the class attribute of the value as error if the Prop has an error" in {
      (Prop("Result", 1, {error("bad"); 1}).toXhtml_! \\("@class")).toString must_== "error"
    }
    "set the class attribute of the value as success if the Prop succeeded" in {
      (Prop("Result", 1, true).toXhtml_! \\("@class")).toString must_== "success"
    }
    "set the class attribute of the value as 'info' if the Prop hasn't been executed" in {
      (Prop("Result", 1, {error("bad"); 1}).toXhtml \\("@class")).toString must_== "info"
    }
    "display the expected value if both actual and expected values are set" in {
      Prop("Result", 1)(2).toXhtml(1) must ==/(<td class="info">2</td>)
    }
    "display the actual value if the expected value is not set" in {
      Prop("Result", 1).toXhtml(1) must ==/(<td class="info">1</td>)
    }
    "display the issue message if there is an issue" in {
      Prop("Result", 1, {fail("failed!"); 1}).toXhtml_!(1).toString must include("failed!")
    }
    "display the expected value in bold if there is an issue" in {
      Prop("Result", 1, {fail("failed!"); 1}).toXhtml_!(1) \\("b") must ==/(<b>1</b>)
    }
    "format a Double expected value with all decimals, up to 15 decimals" in {
      Prop("Result", 1.123456789012345).toXhtml(1) must ==/(<td class="info">1.123456789012345</td>)
    }
    "allow a label decorator to be used to surround the label" in {
      val p = Prop("Result", 1.123456789012345).decorateLabelWith((s: Node) => <b>{s}</b>).toXhtml(0)
      p aka "a prop with a bold decorated label" must ==/(<td><b>Result</b></td>)
    }
    "allow a values decorator to be used to surround the value" in {
      val p = Prop("Result", 1.123456789012345).decorateValueWith((s: Node) => <b>{s}</b>).toXhtml(1)
      p aka "a prop with a bold decorated value" must ==/(<td class="info"><b>1.123456789012345</b></td>)
    }
    "allow an italic/bold/strike value/label decorator to be used to surround the value" in {
      
      "style"                   | "expected"    |
      {(_:ToXhtml).italicValue} ! <i/>          |
      {(_:ToXhtml).boldValue}   ! <b/>          |
      {(_:ToXhtml).strikeValue} ! <s/>          |
      {(_:ToXhtml).italicLabel} ! <i/>          |
      {(_:ToXhtml).boldLabel}   ! <b/>          |
      {(_:ToXhtml).strikeLabel} ! <s/>          |
      {(_:ToXhtml).italic}      ! <i/>          |
      {(_:ToXhtml).bold}        ! <b/>          |
      {(_:ToXhtml).strike}      ! <s/>          |> { (style: ToXhtml => ToXhtml, expected: Elem) => 
         val p = Prop("Result", 1)
         style(p).toXhtml aka "the decorated prop" must have \\(expected)
         val f = Field("Result", 1)
         style(f).toXhtml aka "the decorated field" must have \\(expected)

         val formWithField = new Form { tr(field("Result", 1)) }
         style(formWithField).toXhtml aka "the decorated form with a field" must have \\(expected)

         val formWithProp = new Form { tr(prop("Result", 1)) }
         style(formWithProp).toXhtml aka "the decorated form with a Prop" must have \\(expected)
      }
    }
    "allow a values decorator to be stacked" in {
      val p = Prop("Result", 1.12).boldValue.italicValue.toXhtml(1)
      p aka "a prop with a bold and italic decorated value" must ==/(<td class="info"><i><b>1.12</b></i></td>)
    }
    "allow a to set a success style attribute to decorate the value" in {
      val p = Prop("Result", 1.12).successValue.toXhtml(1)
      p aka "a prop with a success decorated value" must ==/(<td class="success">1.12</td>)
    }
  }
  "A Prop" can {
    "use a different value formatter formatting both missing values and values" in {
      Prop("Result", 1).formatWith((i:Option[Int]) => "["+i.get.toString+"]").toXhtml(1) must ==/(<td class="info">[1]</td>)
    }
    "use a different value formatter formatting existing values" in {
      Prop("Result", 1).formatterIs((i: Int) => "["+i.toString+"]").toXhtml(1) must ==/(<td class="info">[1]</td>)
    }
  }
  Map("Prop" -> Prop("l", 1, None),
      "MatcherProp" -> Prop(1),
      "LineProp" -> LineProp(1)
  ) foreach { c => 
    ("A copied " + c._1) should {
      val p: Prop[_] = c._2
      "copy its value formatter" in {  
        p.formatterIs((s:Any) => "v: " + s.toString)
        p.formattedValue.toString must_== "v: 1"
        val c = p.copy
        c.formattedValue.toString must_== "v: 1"
      }
    }
  }
}
