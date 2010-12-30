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
import org.scalacheck.Gen._
import org.specs.collection.ExtendedList._
import org.specs._

class inSequenceUnit extends SpecificationWithJUnit with TestData with ScalaCheck {
  "A protocol type 'inSequence'" should {
    "consume all if exp=m and rec=m" in {
      inAnyOrder.consume((e), (r)) must verify { t:Result => val (exp, rec) = t
        exp.forall(_.passes) && rec.forall(_.consumed)
      }
    }
    "consume the first exp if exp=m1, m2 and rec=m1" in {
      inAnyOrder.consume(List(e1, e2), (r1)) must verify { t:Result => val (exp, rec) = t
        e1.passes && !e2.passes && r1.consumed
      }
    }
    "consume two exp if exp=m1, m2 and rec=m1, m2" in {
      inAnyOrder.consume(List(e1, e2), List(r1, r2)) must verify { t:Result => val (exp, rec) = t
        exp.forall(_.passes) && rec.forall(_.consumed)
      }
    }
  }
  "not consume received calls if it is a strict sublist of expected calls" in {
    expectedAndReceived must pass { t: Calls => val (expected, received) = t
      inSequence.consume(expected, received)._2 must (((notBeEmpty).when(!expected.isEmpty) or
                                                      (beEmpty).when(expected.isEmpty))).unless(expected.size <= received.size)
    }(set(maxSize->5))
  }
  "consume all received calls if it is a the same list of calls in the same order" in {
    val emptyExpected: List[SpecifiedCall] = Nil
    val emptyReceived: List[ReceivedCall] = Nil

    expectedAndReceived must pass { t: Calls => val (expected, received) = t
      inSequence.consume(expected, received) must be_==((emptyExpected, emptyReceived)).when(expected == received)
    }(set(maxSize->5, maxDiscarded -> 1000))
  }
  "consume all expected calls if they are a prefix of received calls" in {
    expectedAndReceived must pass { t: Calls => val (expected, received) = t
      val receivedStartsWithExpected = received.map(_.method).startsWith(expected.map(_.method))
      val consumedReceived = inSequence.consume(expected, received)._2

      consumedReceived must (notBeEmpty.when(!received.isEmpty && receivedStartsWithExpected) and
                             beEmpty.when(received.isEmpty)).unless(expected.size >= received.size)
    }(set(maxSize->5))
  }
}
