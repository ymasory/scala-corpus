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
import org.specs.runner._

class bagFormSpec extends org.specs.SpecificationWithJUnit {
  case class Person(name: String, age: Int)
  case class PersonLine(name: String, age: Int) extends EntityLineForm[Person] {
    prop((_:Person).name)(name)
    prop((_:Person).age)(age)
  }
  val actual = List(Person("Eric", 36), Person("Bob", 40))
  
  "A bag form" should {
    "match all rows if there are the same number of rows than entities" in {
      val form = new DataTableBagForm("Persons", actual) {
        "Name" | "Age" |
        "Eric" ! 36    | 
        "Bob"  ! 40    | { (name, age) =>
          tr(PersonLine(name, age))
        } 
      }
      (form.execute.toXhtml \\("tr")).toList must have size 4 
    }
    "define, matched/unmatched expected/actual rows" in {
      val form = new DataTableBagForm("Persons", actual) {
        "Name" | "Age" |
        "Eric" ! 36    | 
        "Bob"  ! 42    | { (name, age) =>
          tr(PersonLine(name, age))
        } 
      }
      form.execute.matchedLines aka "matched lines" must have size 2
      form.execute.matchedExpectedLines aka "matched expected lines" must have size 2
      form.execute.matchedActual aka "matched actual lines" must have size 2 
      form.execute.unmatchedExpectedLines aka "unmatched expected lines" must be empty
        
      form.execute.unmatchedActual aka "unmatched actual lines" must be empty
    }
    "report unmatched rows" in {
      val form = new BagForm(actual) {
        th2("Name", "Age")
        tr(PersonLine("Eric", 36)) 
        tr(PersonLine("Bob",  42)) 
      }
      (form.execute.toXhtml \\("tr")).toList must have size 4
    }
    "not match rows twice" in {
      val form = new BagForm(actual) {
        th2("Name", "Age")
        tr(PersonLine("Eric", 36)) 
        tr(PersonLine("Bob",  40)) 
        tr(PersonLine("Eric", 36)) 
      }
      form.execute.expectedLines aka "expected lines" must have size 3
      form.execute.matchedLines aka "matched lines" must have size 2
      form.execute.matchedLines.toString aka "matched lines toString" must include("Eric") and include("Bob")
      form.execute.matchedExpectedLines aka "matched expected lines" must have size 2
      form.execute.matchedActual aka "matched actual lines" must have size 2 
      form.execute.unmatchedExpectedLines aka "unmatched expected lines" must have size 1
    }
    "match the maximum rows" in {
      val form = new BagForm(actual) {
        th2("Name", "Age")
        tr(PersonLine("Eric", 36)) 
        tr(PersonLine("Bob",  40)) 
        tr(PersonLine("Eric", 38)) 
      }
      form.execute.expectedLines aka "expected lines" must have size 3
      form.execute.matchedLines aka "matched lines" must have size 2
      form.execute.matchedLines.toString aka "matched lines toString" must include("Eric") and include("Bob") and include("36")
      form.execute.matchedExpectedLines aka "matched expected lines" must have size 2
      form.execute.matchedActual aka "matched actual lines" must have size 2 
      form.execute.unmatchedExpectedLines aka "unmatched expected lines" must have size 1
    }
    "decorate all fields and properties when decorated" in {
      val form = new BagForm(actual) {
        val p = PersonLine("Eric", 36)
        tr(p)
      }.italic
      form.p.toXhtml must \\("i")
    }
    "format all values according to a value formatter" in {
      val form = new BagForm(actual) {
        val p = PersonLine("Eric", 36)
        tr(p)
        val p2 = PersonLine("Bob", 42)
        tr(p2)
      }.formatterIs(s => "v: " + s.toString)
      form.p.properties(0).asInstanceOf[Prop[_]].formattedValue.toString must_== "v: Eric"
      form.p2.properties(0).asInstanceOf[Prop[_]].formattedValue.toString must_== "v: Bob"
    }
    "format all datatable values according to a value formatter" in {
      val form = new DataTableBagForm("Persons", actual) {
        "Name" | "Age" |
        "Eric" ! 36    | 
        "Bob"  ! 42    | { (name, age) =>
          tr(PersonLine(name, age))
        }
      }.execute.formatterIs(s => "v: " + s.toString)
      form.properties(0).asInstanceOf[Form].properties(0).asInstanceOf[Prop[_]].formattedValue.toString must_== "v: Eric"
    }
  }
  "A bag form" should {
    "not be ok if there are missing actual rows" in {
      val form = new DataTableBagForm("Persons", actual) {
        "Name" | "Age" |
        "Bob"  ! 40    | { (name:String, age:Int) =>
          tr(PersonLine(name, age))
        }
      }
      form.execute.isOk must be (false)
    }
  }
  "A bag form" can {
    "check only a subset of the expected rows with isIncomplete" in {
      val form = new DataTableBagForm("Persons", actual) {
        "Name" | "Age" |
        "Bob"  ! 40    | { (name:String, age:Int) =>
          tr(PersonLine(name, age))
        }
      }
      form.isIncomplete.execute.isOk must be(true)
    }
  }
}
