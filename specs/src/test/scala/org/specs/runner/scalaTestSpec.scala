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

import org.specs.io.mock.MockOutput
import org.specs.specification._
import org.specs._
import org.specs.runner._
import org.specs.Sugar._
import org.scalatest._
import org.scalatest.events._
import org.specs.mock.Mockito
import org.mockito.Mockito._
import scala.collection.immutable._

class scalaTestSpec extends SpecificationWithJUnit with ScalaTestMocks with Contexts {
  "A ScalaTest runner" should {
    "create a ScalaTest suite named after the specification description" in {
      val spec = new SimpleSpecification(that.isOk)
      val suite = new ScalaTestSuite(spec)
      suite.suiteName must be_==(spec.description)
    }
    "create a ScalaTest suite named after the suite class name with no $ sign when created from several specs" in {
      val spec = new SimpleSpecification(that.isOk)
      val suite = new ScalaTestSuite(spec, spec)
      suite.getClass.getName.replaceAll("\\$", "") must include(suite.suiteName)
    }
    "create a ScalaTest nested suite per system under test" in {
      suiteWithGroups.nestedSuites.size must_== 2
    }
    "return groups corresponding to the tags on the specification" in {
      val first = suiteWithGroups.nestedSuites.head
      first.tags must_== Map("have a tag for the second example" -> Set("unit"))
    }
  }
  "A ScalaTest runner"->-(c) should {
    "report failures and errors as test failed, skipped as ignored and the rest as success" in {
      sampleSuite.run(None, reporter, stopper, Filter(), Map(), None, new Tracker())
      got {
        one(reporter).apply(any[SuiteStarting])
        two(reporter).apply(any[TestFailed])
        one(reporter).apply(any[TestIgnored])
        one(reporter).apply(any[TestSucceeded])
        one(reporter).apply(any[SuiteCompleted])
     }
    }
    "use the tags defined on the examples when executing included groups only" in {
      suiteWithGroups.run(None, reporter, stopper, new Filter(Some(Set("unit")), Set()), Map(), None, new Tracker())
      got {
        two(reporter).apply(any[SuiteStarting])
        one(reporter).apply(any[TestSucceeded]) 
        two(reporter).apply(any[SuiteCompleted]) 
	  }
    }
    "use the tags defined on the examples, and not executing excluded groups" in {
      suiteWithGroups.run(None, reporter, stopper, new Filter(None, Set("functional")), Map(), None, new Tracker())
      got {
        two(reporter).apply(any[SuiteStarting])
        two(reporter).apply(any[TestSucceeded]) 
        two(reporter).apply(any[SuiteCompleted]) 
	  }
    }
  }
  def suite(behaviours: that.Value*) = new ScalaTestSuite(new SimpleSpecification(behaviours.toList))
  object sampleSuite extends ScalaTestSuite(sampleSpecification)
  object sampleSpecification extends Specification {
	"the first system" should {
      "skip one example" in { skip("skipped") }
      "have one example ok" in {  1 must_== 1 }
      "have one example ko" in { 1 mustBe 2 }
      "have one example in error" in { throw new Error("error") }
    }
  }
  object suiteWithGroups extends ScalaTestSuite(taggedSpecification)
  object taggedSpecification extends Specification {
    "the first system" should {
      "have no tag for the first example" in { 1 mustBe 1 }
      "have a tag for the second example" in { 1 mustBe 1 } tag "unit"
    } 
    "the second system" should {
      ("have a functional tag for the first example" in { 1 mustBe 1 }) tag "functional"
      "have a functional tag for the second example" in { 1 mustBe 1 } tag "functional"
    }
  }
}
trait ScalaTestMocks extends Mockito { this: BaseSpecification with Contexts =>
   var reporter = mock[org.scalatest.Reporter]
   var stopper = mock[org.scalatest.Stopper]
   val c = beforeContext {
     reporter = mock[org.scalatest.Reporter]
     stopper = mock[org.scalatest.Stopper]
   }
}
/** this class can be used to print out the events that will be applied to a reporter */
case class ReporterSpy(r: org.scalatest.Reporter) extends org.scalatest.Reporter {
  def apply(e: Event) = {
	println("got "+e)
	r.apply(e)
  }
}
