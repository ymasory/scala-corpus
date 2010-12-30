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
import org.scalacheck.util._
import org.specs.mock._
import org.specs.io._
import org.specs.specification._
import scala.collection.immutable
import scala.collection.Set
import org.specs.execute._

class scalacheckMatchersUnit extends MatchersSpecification with ScalaCheckMock with ScalaCheck {
  "The ScalaCheckParameters object" should {
    "provide a 'display' value which is verbose" in {
       display.verbose mustBe true
    }
    "provide a 'display' value which has default values for ScalaCheck parameters" in {
      defaultValues foreach {display(_) mustNotBe null}
    }
    "provide a 'display' case class which can take parameters overriding the default values" in {
      display(minTestsOk->10)(minTestsOk) mustBe 10
      display(minTestsOk->10)(maxDiscarded).toString must_== defaultValues(maxDiscarded).toString
    }
    "provide a 'display' case class which is resilient to a value with a null key" in {
      val s: Symbol = null
      display(s -> 10) must throwA[RuntimeException]
    }
    "provide a 'set' case class which can take parameters overriding the default values" in {
      set(minTestsOk->10)(minTestsOk) mustBe 10
      set(minTestsOk->10)(maxDiscarded).toString must_== defaultValues(maxDiscarded).toString
    }
    "provide a 'set' case class which is not verbose" in {
      set(minTestsOk->10).verbose mustBe false
    }
  }
  "A simple expectation" should {
    "pass if the expectation passes for all values" in {
	  Prop.forAll { i: Int => i must_== i } must pass
	}
	"fail if there is a value for which the expectation fails" in {
      { Prop.forAll { i: Int => i must be_<=(10) } must pass } must throwA[FailureException]
	}
  }
  "The checkFunction method" should {
    "call the forAll function of scalacheck to create a property that will be checked for all generated values" in {
      expect { matcher.forAllProp(any[Gen[Boolean]])(x => Prop.proved) }
      matcher.checkFunction(Gen.value(true))(x => true)(set(minTestsOk->1))
    }
  }
  "The checkScalaCheckProperty method" should {
    "call the printf function of Output to print the results if verbose=true" in {
      expect { matcher.printf(any[String], any[String]) }
      matcher.checkScalaCheckProperty(forAllProp(Gen.value(true))(x => true))(Test.defaultParams, true)
    }
    "call the check function of scalacheck to check the property" in {
      expect { matcher.checkProp(Test.defaultParams, Prop.proved, (s, d) => ()) }
      matcher.checkScalaCheckProperty(forAllProp(Gen.value(true))(x => true))(Test.defaultParams, false)
    }
    "return a true status if the check function return a succeeded result" in {
      expect { matcher.checkProp(any[Test.Params], any[Prop], (s, d) => ()) }
      matcher.checkScalaCheckProperty(forAllProp(Gen.value(true))(x => true))(Test.defaultParams, false)._1 mustBe true
    }
    "return a false status if the check function return a failure" in {
      matcherWithFailure.checkScalaCheckProperty(forAllProp(Gen.value(true))(x => true))(Test.defaultParams, false).success mustBe false
    }
    "return a false status if the check function return a property exception" in {
      matcherWithPropertyException.checkScalaCheckProperty(forAllProp(Gen.value(true))(x => true))(Test.defaultParams, false).success mustBe false
    }
    "return a false status if the check function return an generation exception" in {
      matcherWithGenerationException.checkScalaCheckProperty(forAllProp(Gen.value(true))(x => true))(Test.defaultParams, false).success mustBe false
    }
    "return a false status if the check function return an exhausted status" in {
      matcherWithExhaustedGeneration.checkScalaCheckProperty(forAllProp(Gen.value(true))(x => true))(Test.defaultParams, false).success mustBe false
    }
  }
  "The afterNtries function" should {
    "return tries for 0" in { afterNTries(0) must_== "after 0 tries" }
    "return try for 1" in { afterNTries(1) must_== "after 1 try" }
    "return tries for > 1 tries" in { afterNTries(3) must_== "after 3 tries" }
  }
  "The afterNShrinks function" should {
    "return nothing if there is no shrink" in {
      afterNShrinks(List(Arg("A", "s", 0, "s")(prettyArg))) must_== ""
    }
    "return the original argument and the shrinked one if there is at least a shrink" in {
      afterNShrinks(List(Arg("A", "s", 2, "srt")(prettyArg))) must_== " - shrinked ('srt' -> 's')"
    }
    "return an equal sign if one of the arguments is not shrinked" in {
      afterNShrinks(List(Arg("A", "s", 2, "srt")(prettyArg), Arg("B", "srt", 0, "srt")(prettyArg))) must_== " - shrinked ('srt' -> 's', = )"
    }
  }
}
trait ScalaCheckMock extends Mocker {
  trait ScalaCheckFunctionsMock extends ScalaCheckFunctions {
    def result = Test.Result(Test.Passed, 1, 2, FreqMap.empty[immutable.Set[Any]])
    override def checkProp(params: Test.Params, prop: Prop, printResult: (Int, Int) => Unit) = {
      recordAndReturn(result)
    }
    override def forAllProp[A,P](g: Gen[A])(f: A => Prop): Prop = recordAndReturn(Prop.proved)
  }
  trait ConsoleOutputMock extends Output {
    override def println(s: Any) = record
    override def printf(format: String, args: Any*) = record
  }
  val matcher = new ScalaCheckMatchers with ConsoleOutputMock with ScalaCheckFunctionsMock with DefaultExampleExpectationsListener
  val prettyArg = (a: Any) => org.scalacheck.Pretty.prettyAny(a)

  val matcherWithFailure = new ScalaCheckMatchers with ConsoleOutputMock with ScalaCheckFunctionsMock with DefaultExampleExpectationsListener {
    override def result = Test.Result(Test.Failed(List(Arg("", null, 1, null)(prettyArg)), scala.collection.immutable.Set[String]("label")), 1, 2, FreqMap.empty[immutable.Set[Any]])
  }
  val matcherWithPropertyException = new ScalaCheckMatchers with ConsoleOutputMock with ScalaCheckFunctionsMock with DefaultExampleExpectationsListener {
    override def result = Test.Result(Test.PropException(List(Arg("", null, 2, null)(prettyArg)), FailureException(""), scala.collection.immutable.Set[String]("label")), 1, 2, FreqMap.empty[immutable.Set[Any]])
  }
  val matcherWithGenerationException = new ScalaCheckMatchers with ConsoleOutputMock with ScalaCheckFunctionsMock with DefaultExampleExpectationsListener {
    override def result = Test.Result(Test.GenException(new Exception), 1, 2, FreqMap.empty[immutable.Set[Any]])
  }
  val matcherWithExhaustedGeneration = new ScalaCheckMatchers with ConsoleOutputMock with ScalaCheckFunctionsMock with DefaultExampleExpectationsListener {
    override def result = Test.Result(Test.Exhausted, 1, 2, FreqMap.empty[immutable.Set[Any]])
  }
}
