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
package org.specs.matcher
import org.specs._
import org.specs.Sugar._
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.specs.mock._
import org.specs.io._

class scalacheckMatchersSpec extends MatchersSpecification with ScalaCheckExamples {
  "A 'pass' matcher" should {
    "be ok if a property is true for all generated values" in {
      alwaysTrue must pass(trueFunction)
    }
    "be ok with a true property" in {
      alwaysTrueProp must pass
    }
    "be ok with a true function" in {
      { a: Boolean => true }.forAll must pass
    }
    "be ko with a false property" in {
      expectation(identityProp must pass) must failWithMatch("A counter-example is 'false' \\(after \\d+ tr(y|ies)\\)")
    }
    "be ko if a property is false for a generated value" in {
      expectation(alwaysTrue must pass(falseFunction)) must failWithMatch("A counter-example is 'true' \\(after \\d+ tr(y|ies)\\)")
    }
    "be ko if a expectation is false for a generated value. The failure message should be the assert ko message" in {
      expectation(random must pass(identityAssert)) must failWithMatch("A counter-example is 'false': 'false' is not the same as 'true' \\(after \\d tr(y|ies)\\)")
    }
    "be ko if checking the values generation yields an exception" in {
      expectation(exceptionValues must pass(trueFunction)) must failWithMatch("Exception raised on argument generation")
    }
    "be ko if checking the property yields an exception during its evaluation" in {
      expectation(alwaysTrue must pass(exceptionProperty)) must failWithMatch("Exception raised on property evaluation")
    }
    "be ko if all values have been exhausted before the min number of ok tests is reached" in {
      expectation(Gen.fail[Boolean] must pass(trueFunction)(set(maxDiscarded->10))) must failWith("Gave up after only 0 passed tests. 10 tests were discarded.")
    }
    "accept properties based on scalacheck commands" in  {
      expectation(CounterSpecification must pass) must failWithMatch("A counter-example is .*")
    }
    "display one label in case of a failure" in  {
      expectation((forAll((n: Int) => n == n+1) :| "label") must pass ) must failWithMatch("labels of failing property: label")
    }
    "display several labels in case of a failure" in  {
      import org.scalacheck.Prop._
      val multiply = forAll { (n: Int, m: Int) =>
        val res = n*m
        ("evidence = " + res) |: all(
          "div1" |: m != 0 ==> (res / m == n),
          "div2" |: n != 0 ==> (res / n == m),
          "lt1"  |: res > m,
          "lt2"  |: res > n
        )
      }
      expectation(multiply must pass) must failWithMatch("evidence")
      expectation(multiply must pass) must failWithMatch("lt1")
    }
  }
  val constantPair: Gen[(Double, Double)] = Gen.value((0.0, 0.0))
  "a validate matcher" should {
    "accept a partial function with an untyped partial function returning a SuccessValue" in {
      constantPair must validate { case (x, y) => 1 must_== 1 }
    }
    "accept a partial function with an untyped partial function returning a boolean" in {
      constantPair must validate { case (x, y) => true }
    }
    "be ko if the partial function is false for some value" in {
      expectation(constantPair must validate { case (x, y) => x > 1 }) must failWithMatch("A counter-example is .*")
    }
  }
  "a validate matcher" can {
    "be used with a 'validates' shorthand: gen validates partialFunction" in {
	  constantPair validates { case (x, y) => true }
	}
  }
  "A ScalaCheck property" should {
    "not add new expectations during evaluation if isExpectation is off" in {
      spec.expectationsNb must be_==(101)
    }
    "add new expectations during evaluation if expectProperties is on (default)" in {
      specWithExpectProperties.expectationsNb must be_==(101)
    }
    "not add new expectations during evaluation if dontExpectProperties is on (default)" in {
      specWithDontExpectProperties.expectationsNb must be_==(1)
    }
    "count a new expectation for each time the property is evaluated + one for the pass expectation" in {
      specWithFailure.expectationsNb must be_==(11)
    }
  }
  "Functions" can {
    "be checked directly, without creating an example, using the verifies operator. startsWith" verifies { (a: String, b: String) => (a+b).startsWith(a) }
  }
}
object spec extends Specification with ScalaCheck {
  dontExpectProperties()
  forAll((a:Int) => isExpectation(a == a)) must pass
}
object specWithExpectProperties extends Specification with ScalaCheck {
  forAll((a:Int) => a == a) must pass
}
object specWithDontExpectProperties extends Specification with ScalaCheck {
  dontExpectProperties()
  forAll((a:Int) => a == a) must pass
}
object specWithFailure extends Specification with ScalaCheck {
  expectProperties()
  var counter = 0
  forAll((a:Int) => {counter +=1; counter < 10}) must pass
}

trait ScalaCheckExamples extends ScalaCheck {  this: Specification =>
  val identityProp = forAll((a:Boolean) => a)
  val alwaysTrueProp = proved
  val alwaysTrueFunction: Boolean => Boolean = { a: Boolean => true }
  val alwaysTrue = Gen.value(true)
  val alwaysFalse = Gen.value(false)
  val random = Gen.oneOf(true, false)
  val exceptionValues = Gen(p => throw new java.lang.Exception("e"))
  val trueFunction = ((x: Boolean) => true)
  val partialFunction: PartialFunction[Boolean, Boolean] = { case (x: Boolean) => true }
  val falseFunction = ((x: Boolean) => false)
  val identityAssert: Boolean => Boolean = ((x: Boolean) => x mustBe true)
  val exceptionProperty = ((x: Boolean) => {throw new java.lang.Exception("e"); proved})
}
object CounterSpecification extends Commands {

  val counter = new Counter(0)

  case class State(n: Int)

  def initialState() = {
    counter.reset
    State(counter.get)
  }

  case object Inc extends Command {
    def run(s: State) = counter.inc
    def nextState(s: State) = State(s.n + 1)

    preConditions += (s => true)

    postConditions += ((s,r,u) => counter.get == s.n + 1)
  }

  case object Dec extends Command {
    def run(s: State) = counter.dec
    def nextState(s: State) = State(s.n - 1)
    postConditions += ((s,r,u) => counter.get == s.n - 1)
  }

  def genCommand(s: State): Gen[Command] = Gen.oneOf(Inc, Dec)

}
class Counter(private var n: Int) {
  def inc = n += 1
  def dec = if (n > 3) n else n -= 1
  def get = n
  def reset = n = 0
}
