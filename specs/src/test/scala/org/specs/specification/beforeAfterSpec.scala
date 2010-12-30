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
package org.specs.specification
import org.specs.io.mock.MockOutput
import org.specs.runner._
import org.specs._
import org.specs.runner._

class beforeAfterSpec extends SpecificationWithJUnit {

  "A specification with before clauses" should {
    "have each example using the doBefore method before being executed" in {
      doBeforeExample.execute
      doBeforeExample.messages mustContain "before called 1"
      doBeforeExample.messages mustContain "before called 2"
    }
    "not execute its test if the doBefore method fails" in {
      doBeforeExampleFailing.execute
      doBeforeExampleFailing.messages must containMatch("1 error")
      doBeforeExampleFailing.messages.toList must not containMatch("tested")
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
  "A specification with after clauses" should {
    "have each example using the doAfter method after being executed" in {
      doAfterExample.execute
      doAfterExample.messages mustContain "after called 1"
      doAfterExample.messages mustContain "after called 2"
    }
    "not execute its test if the doAfter method fails" in {
      doAfterExampleFailing.execute
      doAfterExampleFailing.messages must containMatch("1 error")
      doAfterExampleFailing.messages must notContainMatch("tested")
    }
    "work even if the doAfter clause is not declared inside a sus" in {
      object badSpec extends Specification {
        doAfter {}
      }
      badSpec.isOk must beTrue
    }
  }
  "A system under specification" can {
    "specify a doFirst method to setup the context before any example is executed" in {
      specWithDoFirst.execute
      specWithDoFirst.messages.filter(_.startsWith("msg")) must containMatch("doFirst")
      specWithDoFirst.messages.filter(_.startsWith("msg")).drop(1) must (
        containMatch("example 1") and
        containMatch("example 2") and
        notContainMatch("doFirst"))
    }
    "specify a doFirst executing only once " in {
      specWithDoFirstAndNestedExamples.execute
      specWithDoFirstAndNestedExamples.messages.filter(_.contains("msg doFirst")) must haveSize(1)
    }
    "specify a doLast method to setup the context after examples are executed" in {
      specWithDoLast.execute
      specWithDoLast.messages.filter(_.startsWith("msg")) must containMatch("doLast")
      specWithDoLast.messages.filter(_.startsWith("msg")).drop(2) must (
        notContainMatch("example 1") and
        notContainMatch("example 2") and
        containMatch("doLast"))
    }
    "specify a before/after clauses before and after: specification, systems, examples" in {
      specWithAll.execute
      specWithAll.messages.filter(_.startsWith("msg")).toList must_== List(
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
      "msg doLastSpec")
    }
  }
  "A specification" can {
    "use a context to setup the before actions of a system under specification" in {
      specWithBeforeContext.execute
      specWithBeforeContext.beforeIsCalled must beTrue
    }
    "use a context to execute actions around the expectations of examples" in {
      specWithAroundContext.execute
      specWithAroundContext.aroundIsCalled must beTrue
    }
    "use a context to setup the after actions of a system under specification" in {
      specWithAfterContext.execute
      specWithAfterContext.afterIsCalled must beTrue
    }
    "use a context to setup the before and after actions of a system under specification" in {
      specWithContext.execute
      specWithContext.beforeIsCalled must beTrue
      specWithContext.afterIsCalled must beTrue
    }
    "use a repeated context to setup the before and after actions of a system under specification and repeat the same test several times" in {
      specWithRepeatedContext.execute
      specWithRepeatedContext.data must_== 3
    }
    "use an until method to repeat the examples of a sus until a predicate is true" in {
      specWithUntil.execute
      specWithUntil.counter must_== 10
    }
  }
  "A specification with an afterSpec method" should {
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
    "execute the afterSpec method only once after all examples" in {
      var trace = List[String]()
      object s extends Specification with MockOutput {
        doAfterSpec(trace = "afterSpec" :: trace)
        "a system" should {
          "ex1" in { (trace = "ex1" :: trace).isExpectation }
          "ex2" in { (trace = "ex2" :: trace).isExpectation }
        }
      }
      s.reportSpecs
      trace.reverse must_== List("ex1", "ex2", "afterSpec")
    }
  }
  "A specification with nested examples" should {
    "execute the before / after methods only around the leaves examples" in {
      specWithBeforeAfterAndNestedExamples.execute
      specWithBeforeAfterAndNestedExamples.messages.filter(_.startsWith("msg")).mkString("\n", "\n", "\n") must_== List(
      "msg example 1",
        "msg before",
        "msg 1.1",
        "msg after",
        "msg before",
        "msg 1.2",
        "msg after",
      "msg example 2",
        "msg before",
        "msg 2.1",
        "msg after",
        "msg before",
        "msg 2.2",
        "msg after").mkString("\n", "\n", "\n")
    }
    "execute the beforeSpec / afterSpec around the examples" in {
      specWithDoBeforeSpecAfterSpecAndNestedExamples.execute
      specWithDoBeforeSpecAfterSpecAndNestedExamples.messages.filter(_.startsWith("msg")).mkString("\n", "\n", "\n") must_== List(
      "msg beforeSpec",
        "msg example 1",
          "msg 1.1",
          "msg 1.2",
        "msg example 2",
          "msg 2.1",
          "msg 2.2",
      "msg afterSpec").mkString("\n", "\n", "\n")
    }
    "execute the afterSpec even if a composed example fails" in {
      specWithAfterSpecFailingAComposingExample.execute
      specWithAfterSpecFailingAComposingExample.messages.filter(_.startsWith("msg")).mkString("\n", "\n", "\n") must_== List(
      "msg example 1",
      "msg afterSpec").mkString("\n", "\n", "\n")
    }
  }
}

abstract class beforeAfterSpecification extends Specification with Contexts with Console with MockOutput { 
  shareVariables()
  def execute = { systemsList = Nil; executeSpec }
  def executeSpec
  override val specs = List(this)
  override def main(args: Array[String]) = super[Console].main(args)
}

object doBeforeExample extends beforeAfterSpecification {
  override def executeSpec = {
    var beforeCalls = 0
    "A specification" should { doBefore { beforeCalls += 1; println("before called " + beforeCalls) }
      "have example 1 ok" in { true }
      "have example 2 ok" in { true }
    }
    reportSpecs
  }
}
object doBeforeExampleFailing extends beforeAfterSpecification {
  override def executeSpec = {
      var beforeCalls = 0
    "A specification" should { 
      doBefore { error("before error") }
      "have example 1 ok" in {  }
    }
    reportSpecs
  }
}
object doAfterExample extends beforeAfterSpecification {
  override def executeSpec = {
    var afterCalls = 0
    "A specification" should { doAfter { afterCalls += 1; println("after called " + afterCalls) }
      "have example 1 ok" in { true }
      "have example 2 ok" in { true }
    }
    reportSpecs
  }
}
object doAfterExampleFailing extends beforeAfterSpecification {
  override def executeSpec = {
      var afterCalls = 0
    "A specification" should { doAfter { println("after");error("after error") }
      "have example 1 ok" in {  }
    }
    reportSpecs
  }
}
object specWithBeforeContext extends beforeAfterSpecification {
  var beforeIsCalled = false
  val context1 = beforeContext {
    beforeIsCalled = true 
  }
  override def executeSpec = {
    "A specification" ->- context1 should {
      "have example 1 ok" in { 1 must_== 1 }
    }
    reportSpecs
  }
}
object specWithAroundContext extends beforeAfterSpecification {
  var aroundIsCalled = false
  val context1 = aroundExpectationsContext {
    (a: Any) => { aroundIsCalled = true; a } 
  }
  override def executeSpec = {
    "A specification" ->- context1 should {
      "have example 1 ok" in { 1 must_== 1 }
    }
    reportSpecs
  }
}
object specWithAfterContext extends beforeAfterSpecification {
  var afterIsCalled = false
  val context1 = afterContext(afterIsCalled = true)
  override def executeSpec = {
    "A specification" ->- context1 should {
      "have example 1 ok" in { }
    }
    reportSpecs
  }
}
object specWithContext extends beforeAfterSpecification {
  var beforeIsCalled = false
  var afterIsCalled = false
  val context1 = context(beforeIsCalled = true, afterIsCalled = true)
  override def executeSpec = {
    "A specification" ->- context1 should {
      "have example 1 ok" in { }
    }
    reportSpecs
  }
}
object specWithRepeatedContext extends beforeAfterSpecification {
  var data = 0
  override def executeSpec = {
  val context1 = beforeContext(data += 1).until(data >= 3)
    "A specification with repeated context" ->- context1 should {
      "have example 1 ok" in { 1 must_== 1 }
    }
    reportSpecs
  }
}
object specWithUntil extends beforeAfterSpecification {
  var counter = 0
  override def executeSpec = {
    "A specification" should {
      until(counter == 10)
      "have example 1 ok" in { counter += 1 }
    }
    reportSpecs
  }
}
object specWithDoFirst extends beforeAfterSpecification {
  override def executeSpec = {
    "A specification" should {
      doFirst { println("msg doFirst") }
      "have example 1 ok" in { println("msg example 1") }
      "have example 2 ok" in { println("msg example 2") }
    }
    reportSpecs
  }
}
object specWithDoFirstAndNestedExamples extends beforeAfterSpecification {
  override def executeSpec = {
    "A specification" should {
      doFirst { println("msg doFirst") }
      "have example 1 ok" in { 
        "have example 1.1 ok" in { println("msg example 1.1") } 
        "have example 1.2 ok" in { println("msg example 1.2") } 
      }
      "have example 2 ok" in { 
        "have example 2.1 ok" in { println("msg example 2.1") } 
        "have example 2.2 ok" in { println("msg example 2.2") } 
      }
    }
    reportSpecs
  }
}
object specWithDoLast extends beforeAfterSpecification {
  override def executeSpec = {
    "A specification" should {
      doLast { println("msg doLast") }
      "have example 1 ok" in { println("msg example 1") }
      "have example 2 ok" in { println("msg example 2") }
    }
    reportSpecs
  }
}
object specWithAll extends beforeAfterSpecification {
  override def executeSpec = {
    doBeforeSpec { println("msg doFirstSpec") }
    "A specification" should {
      doFirst 	{ println("msg doFirstSus1") }
      doBefore 		{ println("msg doBeforeSus1") }
      doLast 	{ println("msg doLastSus1") }
      doAfter 		{ println("msg doAfterSus1") }
      "have example 1.1 ok" in { println("msg example 1.1") }
      "have example 1.2 ok" in { println("msg example 1.2") }
    }
    "A specification" should {
      println("msg doFirstSus2").doFirst
      println("msg doBeforeSus2").before
      "have example 2.1 ok" in { println("msg example 2.1") }
      "have example 2.2 ok" in { println("msg example 2.2") }
      println("msg doAfterSus2").after
      println("msg doLastSus2").doLast
    }
    println("msg doLastSpec").afterSpec
    reportSpecs
  }
}
object specWithBeforeAfterAndNestedExamples extends beforeAfterSpecification {
  override def executeSpec = {
    "A specification" should {
      doBefore { println("msg before") }
      doAfter  { println("msg after") }
      "example 1" in { 
        println("msg example 1")
        "1.1" in { println("msg 1.1") }
        "1.2" in { println("msg 1.2") }
      }
      "example 2" in { 
        println("msg example 2")
        "2.1" in { println("msg 2.1") }
        "2.2" in { println("msg 2.2") }
      }
    }
    reportSpecs
  }
}
object specWithDoBeforeSpecAfterSpecAndNestedExamples extends beforeAfterSpecification {
  override def executeSpec = {
    doBeforeSpec { println("msg beforeSpec") }
    doAfterSpec { println("msg afterSpec") }
    "A specification" should {
      "example 1 ok" in { 
        println("msg example 1")
        "have example 1.1 ok" in { println("msg 1.1") } 
        "have example 1.2 ok" in { println("msg 1.2") } 
      }
      "msg example 2 ok" in { 
        println("msg example 2")
        "have example 2.1 ok" in { println("msg 2.1") } 
        "have example 2.2 ok" in { println("msg 2.2") } 
      }
    }
    reportSpecs
  }
}
object specWithAfterSpecFailingAComposingExample extends beforeAfterSpecification {
  override def executeSpec = {
    doAfterSpec { println("msg afterSpec") }
    "A specification" should {
      "example 1 ok" in { 
        println("msg example 1")
        1 must_== 2
        "have example 1.1 ok" in { println("msg 1.1") } 
      }
    }
    reportSpecs
  }
}
