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
package org.specs.mock;
import org.specs.runner._
import org.specs.Sugar._
import org.specs.mock._
import org.scalacheck.Gen._
import org.specs.collection.ExtendedList._
import org.specs._

class numberOfMessagesUnit extends SpecificationWithJUnit with TestData with ScalaCheck {
  "A protocol type 'numberOfMessages'" should {
    "exactly 2: consume all if exp=m and rec=m, m" in {
      new inAnyOrder(exactlyN(2)).consume((e), List(r, rprime)) must verify { t:Result => val (exp, rec) = t
        exp.forall(_.passes) && rec.forall(_.consumed)
      }
    }
    "exactly 2: not pass the expected call if exp=m and rec=m, but consume the received call" in {
      new inAnyOrder(exactlyN(2)).consume(List(e), List(r)) must_== (List(e), List(r))
      new inAnyOrder(exactlyN(2)).consume((e), List(r)) must verify { t:Result => val (exp, rec) = t
        exp.forall(!_.passes) && rec.forall(_.consumed)
      }
    }
  }
  "An exactly(N) protocol type" should {
    val exactly2 = new inAnyOrder(exactlyN(2))
    "not pass expected calls at all if there are less calls than expected" in {
      expectedAndReceived must pass { t: Calls => val (expected, received) = t
        exactly2.consume(expected, received)
        expected.forall(_.passes) must be(false).unless(expected.isEmpty || received.isEmpty || received.size >= expected.size)
      }(set(maxSize->5))
    }
    "consume all expected and received calls if it is a multiple of the expected calls" in {
      sameCalls must pass { t: Calls => val (expected, received) = t
        exactly2.consume(expected, received:::(received.map((r: ReceivedCall)=>ReceivedCall(r.method))))
        expected.forall(_.passes) must be(true).unless(expected.isEmpty || received.isEmpty)
      }(set(maxSize->5))
    }
    "not pass the expected calls if the received calls are not an exact multiple of the expected calls" in {
      sameCalls must pass { t: Calls => val (expected, same) = t
        exactly2.consume(expected, same:::same:::same)
        expected.forall(_.passes) must be(false).unless(expected.isEmpty || same.isEmpty)
      }(set(maxSize->5))
    }
  }
}
