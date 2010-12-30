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
package org.specs

import org.specs._
import org.specs.matcher._
import org.specs.Sugar._
import org.specs.runner._
import org.specs.util._
import org.specs.util.ExtendedThrowable._
import scala.collection.mutable._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.specs.matcher.MatcherUtils._
import org.specs.specification._

class specificationsUnit extends SpecificationWithJUnit with ScalaCheck {

  "A specification" should {
    "have a description corresponding to its unqualified class name, whatever the class name" in {
      def classNames = for {
        packageName <- Gen.oneOf("com", "scala")
        className <- Gen.oneOf(packageName + "s", packageName + ".specs", packageName + ".other.normal")
        name <- Gen.oneOf(className, className + "$inner", className + "$inner$", className + "$2", className + "$2$")
      } yield name

      classNames must pass { name : String =>
        specification.createDescription(name) must (not(beMatching("\\$")) and
                                           not(beMatching("\\.")) and
                                           not(beInt))
      }
    }
  }
  "A specification with one sus and an example alone" should {
    object s extends specWith2Sus
    "have two sus" in {
      s.systems must have size(2)
      s.systems(0).examples must have size(1)
      s.systems(0).examples(0).description must_== "ex1" 
      s.systems(1).examples must have size(1)
      s.systems(1).examples(0).description must_== "ex2" 
    }
  }
  "A specification with one expectation only" should {
    "create a default sus" in {
      nudeSpecification.systems.size mustBe 1
    }
    "create a default example" in {
      nudeSpecification.systems.head.examples.size mustBe 1
    }
    "create a default example named 'example 1'" in {
      nudeSpecification.systems.head.examples.head.description must_== "example 1"
    }
    "count 1 expectation" in {
      nudeSpecification.expectationsNb mustBe 1
    }
  }
  "A specification with 2 nested sus" should {
	object s extends Specification with io.mock.MockOutput { "it" should { specify("have a nested sus that") should { "work" in {} }} }
	"be executed as nested examples" in {
	  s.reportSpecs.systems(0).errors(0) must haveClass[SpecificationBuildException]
	}
  }
  "the location of a failure" should {
    val startLine = 113
    "indicate the precise location if it is an anonymous example" in {
      anonymousSpecification.failures(0).location must_== "specificationsUnit.scala:" + startLine
    }
    "indicate the precise location if it is in a sus" in {
      failedSpecification.failures(0).location must_== "specificationsUnit.scala:" + (startLine + 1)
    }
    "indicate the precise location if it is a skipped example" in {
      skippedSpecification.skipped(0).location must_== "specificationsUnit.scala:" + (startLine + 2)
    }
    "indicate the precise location if it is a skipped example with a skipped matcher" in {
      skippedMatcherSpecification.skipped(0).location must_== "specificationsUnit.scala:" + (startLine + 3)
    }
    "indicate the precise location if it is in an example" in {
      failedSpecification.failures(0).getMessage must_== "'1' is not equal to '0'"
      failedSpecification.failures(0).location must_== "specificationsUnit.scala:" + (startLine + 1)
    }
  }
  "A specification with 2 expectations only" should {
    "create 2 default examples with a normal assert" in {
      twoNamedExamples.systems.head.examples.size mustBe 2
    }
    "create 2 default examples with a named assert" in {
      twoExamples.systems.head.examples.size mustBe 2
    }
  }
  def isInt(s: String): Boolean = {try {s.toInt} catch {case _ => return false}; true}
  def beInt = new Matcher[String](){
    def apply(s: => String) = (isInt(s), q(s) + " is an integer", q(s) + " is not an integer")
  }
  object specification extends Specification
}
object anonymousSpecification extends Specification { 1 must_== 0 }
object failedSpecification extends Specification { "it" should { 1 must_== 0; "" in {} } }
object skippedSpecification extends Specification { "it" should { "be skipped" in { skip("be skipped") } } }
object skippedMatcherSpecification extends Specification { "it" should { "be skipped" in { 1 must be_==(0).orSkipExample } } }

class specWith2Sus extends Specification { 
  "this sus" should { "ex1" in { 1 must_== 1 } }
  "ex2" in { 1 must_== 1 }
}
object nudeSpecification extends Specification { "name" mustEqual "name"; systems }
object twoNamedExamples extends Specification {
  val n = "name" aka "the string"
  n mustEqual "name"
  n mustEqual "name2"
  systems
}
object twoExamples extends Specification {
  "name" mustEqual "name"
  "name" mustEqual "name2"
}
    