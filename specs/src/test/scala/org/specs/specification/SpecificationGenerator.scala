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
import org.scalacheck.Gen.{ choose, sized, listOfN, listOf }
import org.scalacheck._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.specs.matcher._
import org.specs._

trait SpecificationGenerator { self: Specification =>
  object spec extends Specification("generated spec")

  def genExpectation = for(value <- choose(0, 4)) yield { () =>
    value match {
      case 0 | 1 => spec.theValue(value) must_== 1
      case 2     => spec.skip("this is a skipped example")
      case 3     => error("error in the example")
    }
  }

  def genExample(sus: Sus) = for (a <- genExpectation) yield {
    val newExample = new Example("generated example", sus)
    sus.addExample(newExample)
    newExample.in { a() }
  }

  def genSizedSus(size: Int): Gen[Sus] = genSizedSus(size, spec)
  def genSizedSus(size: Int, s: Specification): Gen[Sus] = {
    val sus = new Sus("sus with " + size + " max examples", s)
    for { n <- choose(0, size)
          e <- listOf(n, genExample(sus))
    } yield sus
  }
  def genSus = sized(size => genSizedSus(size))
  def genSizedSpec(size: Int): Gen[Specification] = {
    val generatedSpec = new Specification("spec with " + size + " max sus") {}
    for { systemsNb <- choose(0, size)
          systems <- listOfN(systemsNb, genSizedSus(size, generatedSpec))
          subSpecsNb <- choose(0, 1)
          subSpecs <- listOfN(subSpecsNb, genSizedSpec(size))
    } yield {
      subSpecs.foreach(generatedSpec.include(_))
      generatedSpec
    }
  }
  def genSpec = sized(size => genSizedSpec(size))

  implicit val arbitrarySus: Arbitrary[Sus] = Arbitrary { genSus }
  implicit val arbitrarySpec: Arbitrary[Specification] = Arbitrary { genSpec }
}
object generatorSpecification extends Specification with SpecificationGenerator with ScalaCheck {
  "a sus" should {
    "have a number of error + failure + successes + skipped == the number of examples" in {
      forAll {(sus: Sus) =>
        (sus.failures.size + sus.errors.size + sus.skipped.size + sus.successes.size) must be_==(sus.examples.size)
      } must pass(set(maxSize -> 5))
    }
  }
  "a specification" should {
    "have a number of error + failure + successes + skipped == the number of examples" in {
      forAll {(spec: Specification) =>
        (spec.failures.size + spec.errors.size + spec.skipped.size + spec.successes.size) must be_==(spec.examples.size)
      } must pass(set(maxSize -> 5))
    }
  }
}
