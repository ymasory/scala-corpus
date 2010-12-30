/*
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
package org.specs.specification
import org.specs.io.mock.MockOutput
import org.specs.runner._
import org.specs._
import org.specs.runner._

class beforeAfterDontShareSpec extends specWithBeforeAfterExamples {
  "A specification with before clauses" should { msgs.clear.before
    "have each example using the doBefore method before being executed" in {
      doBeforeExample.reportSpecs
      msgs().filter(_.startsWith("msg")).toList must_== List(
        "msg before", 
        "msg before")
    }
    "not execute its test if the doBefore method has an error" in {
      doBeforeExampleWithError.reportSpecs
      msgs() must containMatch("error")
      msgs() must not containMatch("executed")
    }
    "report failed expectations in the doBefore clause" in {
      doBeforeExampleFailing.reportSpecs
      doBeforeExampleFailing.failures.map(_.toString) must containMatch("'1' is not equal to '2'")
      msgs() must not containMatch("executed")
    }
    "be executed even if the doBefore clause is not declared inside a sus" in {
      object badSpec extends Specification {
        doBefore {}
      }
      badSpec.isOk must beTrue
    }
    "stack the doBefore actions by default" in {
      object s extends Specification with MockOutput {
        shareVariables()
        var i = ""
        "this system" should { 
          doBefore(i += "a") 
          doBefore(i += "b")
          "stack before methods" in { 1 must_== 1 }
        }
      }
      s.reportSpecs.i must_== "ab"
    }
  }
  "A specification with after clauses" should { msgs.clear.before
    "have each example using the doAfter method after being executed" in {
      doAfterExample.reportSpecs
      msgs().filter(_.startsWith("msg")).toList must_== List(
        "msg after", 
        "msg after")
    }
    "work even if the doAfter clause is not declared inside a sus" in {
      object badSpec extends Specification {
        doAfter {}
      }
      badSpec.isOk must beTrue
    }
  }
  "A system under specification" can { msgs.clear.before
    "specify a doFirst method to setup the context before any example is executed" in {
      specWithDoFirst.reportSpecs
      msgs().filter(_.startsWith("msg")) must containMatch("doFirst")
      msgs().filter(_.startsWith("msg")).drop(1) must (
        containMatch("example 1") and
        containMatch("example 2") and
        notContainMatch("doFirst"))
    }
    "specify a doFirst executing only once " in {
      specWithDoFirstAndNestedExamples.reportSpecs
      msgs().filter(_.contains("msg doFirst")) must have size(1)
    }
    "specify a doLast method to setup the context after examples are executed" in {
      specWithDoLast.reportSpecs
      msgs().filter(_.startsWith("msg")) must containMatch("doLast")
      msgs().filter(_.startsWith("msg")).drop(2) must (
        notContainMatch("example 1") and
        notContainMatch("example 2") and
        containMatch("doLast"))
    }
    "specify a before/after clauses before and after: specification, systems, examples" in {
      specWithAll.reportSpecs
      msgs().filter(_.startsWith("msg")).toList.mkString("\n", "\n", "\n") must_== List(
      "msg doFirstSpec",
        "msg doFirstSus1",
          "msg doBeforeSus1",
            "msg example 1.1",
          "msg doAfterSus1",
          "msg doBeforeSus1",
            "msg example 1.2",
          "msg doAfterSus1",
        "msg doLastSus1",

        "msg doFirstSus2",
          "msg doBeforeSus2",
            "msg example 2.1",
          "msg doAfterSus2",
          "msg doBeforeSus2",
            "msg example 2.2",
          "msg doAfterSus2",
        "msg doLastSus2",
      "msg doLastSpec").mkString("\n", "\n", "\n")
    }
  }
  "A specification with an afterSpec method" should { msgs.clear.before
    "execute the afterSpec method even if the spec execution is sequential - see issue 134" in {
      var after = false
      object s extends Specification with MockOutput {
        "a system" should {
          setSequential()
          "with an example" in { 1.isExpectation }
        }
        (after = true).afterSpec
      }
      s.reportSpecs
      after must beTrue
    }
  }
  "A specification with nested examples" should { msgs.clear.before
    "execute the before / after methods only around the leaves examples" in {
      specWithBeforeAfterAndNestedExamples.reportSpecs
      msgs().filter(_.startsWith("msg")).mkString("\n", "\n", "\n") must_== List(
      "msg example 1",
        "msg before",
        "msg 1.1",
        "msg after",
        "msg example 1",
        "msg before",
        "msg 1.2",
        "msg after",
      "msg example 2",
        "msg example 2",
        "msg before",
        "msg 2.1",
        "msg after",
        "msg example 2",
        "msg before",
        "msg 2.2",
        "msg after").mkString("\n", "\n", "\n")
    }
    "execute the beforeSpec / afterSpec around the examples" in {
      specWithDoBeforeSpecAfterSpecAndNestedExamples.reportSpecs
      msgs().filter(_.startsWith("msg")).mkString("\n", "\n", "\n") must_== List(
      "msg beforeSpec",
        "msg example 1",
          "msg 1.1",
          "msg example 1",
          "msg 1.2",
        "msg example 2",
          "msg example 2",
          "msg 2.1",
          "msg example 2",
          "msg 2.2",
      "msg afterSpec").mkString("\n", "\n", "\n")
    }
    "execute the afterSpec even if a composed example fails" in {
      specWithAfterSpecFailingAComposingExample.reportSpecs
      msgs().filter(_.startsWith("msg")).mkString("\n", "\n", "\n") must_== List(
      "msg example 1",
      "msg afterSpec").mkString("\n", "\n", "\n")
    }
  }
}
trait specWithBeforeAfter extends Specification with MockOutput {
  dontShareVariables
}
object msgs {
  var messages: scala.collection.mutable.ListBuffer[String] = new scala.collection.mutable.ListBuffer[String]
  def apply() = messages.toList
  def add(m: String) = messages.append(m)
  def clear = messages.clear
}
class specWithBeforeAfterExamples extends SpecificationWithJUnit {
  
  object doBeforeExample extends specWithBeforeAfter {
    "A specification" should { 
      msgs.add("msg before").before
      "have example 1 ok" in { true must_== true }
      "have example 2 ok" in { true must_== true }
    }
  }
  object doBeforeExampleWithError extends specWithBeforeAfter {
    "A specification" should { 
      doBefore { error { msgs.add("error"); "before error"} }
      "have example 1 ok" in { msgs.add("executed") }
    }
  }
  object doBeforeExampleFailing extends specWithBeforeAfter {
    "A specification" should { 
      doBefore { 1 must_== 2 }
      "have example 1 ok" in { msgs.add("executed") }
    }
  }
  object doAfterExample extends specWithBeforeAfter {
    "A specification" should { doAfter { msgs.add("msg after") }
      "have example 1 ok" in { true }
      "have example 2 ok" in { true }
    }
  }
  object specWithDoFirst extends specWithBeforeAfter {
    "A specification" should {
      doFirst { msgs.add("msg doFirst") }
      "have example 1 ok" in { msgs.add("msg example 1") }
      "have example 2 ok" in { msgs.add("msg example 2") }
    }
  }
  object specWithDoFirstAndNestedExamples extends specWithBeforeAfter {
    "A specification" should {
      doFirst { msgs.add("msg doFirst") }
      "have super example 1 ok" in { 
        "have nested example 1.1 ok" in { msgs.add("msg example 1.1") } 
        "have nested example 1.2 ok" in { msgs.add("msg example 1.2") } 
      }
      "have super example 2 ok" in { 
        "have nested example 2.1 ok" in { msgs.add("msg example 2.1") } 
        "have nested example 2.2 ok" in { msgs.add("msg example 2.2") } 
      }
    }
  }
  object specWithDoLast extends specWithBeforeAfter {
    "A specification" should {
      doLast { msgs.add("msg doLast") }
      "have example 1 ok" in { msgs.add("msg example 1") }
      "have example 2 ok" in { msgs.add("msg example 2") }
    }
  }
  object specWithAll extends specWithBeforeAfter {
    doBeforeSpec { msgs.add("msg doFirstSpec") }
    "A specification" should {
      doFirst 	{ msgs.add("msg doFirstSus1") }
      doBefore 		{ msgs.add("msg doBeforeSus1") }
      doLast 	{ msgs.add("msg doLastSus1") }
      doAfter 		{ msgs.add("msg doAfterSus1") }
      "have example 1.1 ok" in { msgs.add("msg example 1.1") }
      "have example 1.2 ok" in { msgs.add("msg example 1.2") }
    }
    "A specification" should {
      msgs.add("msg doFirstSus2").doFirst
      msgs.add("msg doBeforeSus2").before
      "have example 2.1 ok" in { msgs.add("msg example 2.1") }
      "have example 2.2 ok" in { msgs.add("msg example 2.2") }
      msgs.add("msg doAfterSus2").after
      msgs.add("msg doLastSus2").doLast
    }
    msgs.add("msg doLastSpec").afterSpec
  }
  object specWithBeforeAfterAndNestedExamples extends specWithBeforeAfter {
    "A specification" should {
      doBefore { msgs.add("msg before") }
      doAfter  { msgs.add("msg after") }
      "example 1" in { 
        msgs.add("msg example 1")
        "1.1" in { msgs.add("msg 1.1") }
        "1.2" in { msgs.add("msg 1.2") }
      }
      "example 2" in { 
        msgs.add("msg example 2")
        "2.1" in { msgs.add("msg 2.1") }
        "2.2" in { msgs.add("msg 2.2") }
      }
    }
  }
  object specWithDoBeforeSpecAfterSpecAndNestedExamples extends specWithBeforeAfter {
    doBeforeSpec { msgs.add("msg beforeSpec") }
    doAfterSpec { msgs.add("msg afterSpec") }
    "A specification" should {
      "example 1 ok" in { 
        msgs.add("msg example 1")
        "have example 1.1 ok" in { msgs.add("msg 1.1") } 
        "have example 1.2 ok" in { msgs.add("msg 1.2") } 
      }
      "msg example 2 ok" in { 
        msgs.add("msg example 2")
        "have example 2.1 ok" in { msgs.add("msg 2.1") } 
        "have example 2.2 ok" in { msgs.add("msg 2.2") } 
      }
    }
  }
  object specWithAfterSpecFailingAComposingExample extends specWithBeforeAfter {
    doAfterSpec { msgs.add("msg afterSpec") }
    "A specification" should {
      "example 1 ok" in { 
        msgs.add("msg example 1")
        1 must_== 2
        "have example 1.1 ok" in { msgs.add("msg 1.1") } 
      }
    }
  }
}