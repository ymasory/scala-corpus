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
package org.specs.runner
import org.specs.literate._
import org.specs._
import org.specs.specification._

class tagsSpec extends HtmlSpecificationWithJUnit with Fixtures {

<t>Tags can be attached to examples to classify them.

  The 2 main use cases for using tags are:
    1. in a system under test, exclude all other examples to run only one when trying to diagnose why that example is failing
    2. run only some examples across different specifications. For example, all the examples related to
      a given financial product in a financial application

1. Exclude all other examples with tags

  Let's define a specification with several examples:
{"""
    object mySpec extends Specification {
      "example 1" in { 1 must_== 1 }
      "example 2" in { 2 must_== 2 }
    }
"""}
  The second example can be added a tag: "only this":
{"""
     "example 2" in { 2 must_== 2 } tag("only this")  """}

  In that case, it is possible to parametrize the specification with tags, so that
  <ex>only the examples with those tags will be run, other examples will be skipped</ex>:
{"""
    object mySpec extends Specification {
      "example 1" in { 1 must_== 1 }
      "example 2" in { 2 must_== 2 } tag("only this")
    }
    mySpec accepts ("only this")  """ }{onlyTaggedExamples}

  This will output:
 { specOutput }

  <ex>If a tag is applied to a sus, it must not applied to all its examples</ex>:
{"""
    object specWithSus extends Specification {
      "this sus" should {
        "be tagged 1" in { 1 must_== 1 }
        "be tagged 2" in { 1 must_== 1 }
      } tag("be tagged")
    } """ }{susExamplesAreNotTagged}

</t> isSus
}
trait Fixtures { this: LiterateSpecification =>
   object mySpec extends Specification with ScalaCheck {
     "example 1" in { 1 must_== 2 }
     "example 2" in {
       1 must_== 1
     } tag("only this")
   }
   mySpec acceptTag ("only this")
   def onlyTaggedExamples = eg {
     val acceptedExamples = mySpec.systems.flatMap(_.examples).filter(_.isAccepted)
     acceptedExamples.size must_== 1
     acceptedExamples.head.description must_== "example 2"
   }
   object specWithSus extends Specification {
     "this sus" should {
        "be tagged 1" in { 1 must_== 1 }
        "be tagged 2" in { 1 must_== 1 }
      } tag("be tagged")
    }
   def susExamplesAreNotTagged = eg {
     specWithSus.systems.flatMap(_.examples).flatMap(_.tagNames) must beEmpty
   }
   import org.specs.io.mock.MockOutput
   def specOutput = {
     val runner = new ConsoleRunner(mySpec) with MockOutput
     runner.reportSpecs
     consoleOutput("\n", runner.messages)
   }
}
