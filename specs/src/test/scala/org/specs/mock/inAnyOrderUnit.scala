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
package org.specs.mock
import org.specs.runner._
import org.specs.Sugar._
import org.specs.mock._
import org.scalacheck.Prop._
import org.scalacheck.Gen._
import org.specs.collection.ExtendedList._
import org.specs._

class inAnyOrderUnit extends SpecificationWithJUnit with TestData with ScalaCheck {
  "A protocol type 'inAnyOrder'" should {
    "consume nothing if exp=m and rec=nil" in {
      inAnyOrder.consume((e), ()) must verify { t:Result => val (exp, rec) = t
        exp.forall(!_.passes) && rec.isEmpty
      }
    }
    "consume all if exp=m and rec=m" in {
      inAnyOrder.consume((e), (r)) must verify { t:Result => val (exp, rec) = t
        exp.forall(_.passes) && rec.forall(_.consumed)
      }
    }
    "consume one exp if exp=m1, m2 and rec=m2" in {
      inAnyOrder.consume(List(e1, e2), (r2)) must verify { t:Result => val (exp, rec) = t
        !e1.passes && e2.passes && rec.forall(_.consumed)
      }
    }
    "consume two exp if exp=m1, m2 and rec=m2, m1" in {
      inAnyOrder.consume(List(e1, e2), List(r2, r1)) must verify { t:Result => val (exp, rec) = t
        exp.forall(_.passes) && rec.forall(_.consumed)
      }
    }
    "consume only one received message if exp=m and rec=m, m" in {
      inAnyOrder.consume(List(e), List(r, r1)) must verify { t:Result => val (exp, rec) = t
        exp.forall(_.passes) && r.consumed && !r1.consumed
      }
    }
    "not consume all expected calls if it is a strict sublist of expected calls" in {
      expectedAndReceived must pass { t: Calls => val (expected, received) = t
        inAnyOrder.consume(expected, received)
        expected.forall(_.passes) must beFalse.unless(expected.isEmpty || received.isEmpty || (expected.size <= received.size))
      }(set(maxSize->5))
    }
    "consume all received calls if it is a the same list of calls in a different order" in {
      expectedAndReceived must pass { t: Calls => val (expected, received) = t
        inAnyOrder.consume(expected, received)
        expected.forall(_.passes) must beTrue.unless(expected.size != received.size)
        received.forall(_.consumed) must beTrue.unless(expected.size != received.size)
      }(set(maxSize->5, maxDiscarded->1000))
    }
    "consume all expected calls if the received calls are a the superset of the expected calls" in {
      expectedAndReceived must pass { t: Calls => val (expected, received) = t
	    inAnyOrder.consume(expected, received)
		expected.forall(_.passes) must beTrue.unless(received.isEmpty || expected.size >= received.size)
      }(set(maxSize->5))
    }
  }
}
import org.scalacheck.Gen
trait TestData extends ProtocolTypes {
  val (e, e1, e2) = (ExpectedCall("m"), ExpectedCall("m1"), ExpectedCall("m2"))
  val (r, r1, r2, rprime) = (ReceivedCall("m"), ReceivedCall("m1"), ReceivedCall("m2"), ReceivedCall("m"))
  val exactly2Calls = new ProtocolDef(twoOf, List(e))
  type Result = (List[SpecifiedCall], List[ReceivedCall])
  val methods = List("m1", "m1", "m2", "m3")
  def clearCalls = {
    (e, e1, e2).foreach(_.callsNumber = 0)
    (r, r1, r2).foreach(_.consumedBy = None)
  }
  def sameCalls = for (expected <- listOf(Gen.oneOf(methods)))
                    yield (expected.map(new ExpectedCall(_)), expected.scramble.map(new ReceivedCall(_)))

  def expectedAndReceived = {
    for { expected <- listOf(Gen.oneOf(methods))
          n <- choose(-2, 2)
	} yield (expected.map(new ExpectedCall(_)), (expected.scramble:::expected.scramble).take(expected.size + n).map(new ReceivedCall(_)))
  }
  type Calls = (List[ExpectedCall], List[ReceivedCall])

}