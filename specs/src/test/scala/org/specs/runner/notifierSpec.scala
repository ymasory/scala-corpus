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
import org.specs._
import org.specs.specification._
import org.specs.runner._
import org.specs.mock.Mockito
import org.specs.execute._

class notifierSpec extends SpecificationWithJUnit with Mockito {
  var notifier = mock[Notifier]
  "A notifier for a specification" should beNotifiedOf { 
    new NotifierRunner(s, notifier).reportSpecs
    "the start of a run with the total number of examples" in {
      there was one(notifier).runStarting(5)
    }
    "the start of a system" in {
      there was one(notifier).systemStarting("system1 should")
    }
    "the failure of a system as a system failure with no details" in {
      there was one(notifier).systemFailed(be_==("system1 should"), be_==("") ^^ ((_:Exception).getMessage))
    }
    "the failure of a system as an example failure with details" in {
      there was one(notifier).exampleFailed("system failure", new FailureException("sus failed")) 
    }
    "the error of a system as an example error with details" in {
      there was one(notifier).exampleError(be_==("system error"), be_==("sus error") ^^ ((_:Exception).getMessage)) 
    }
    "the start of an example" in {
      there was one(notifier).exampleStarting("ex1-1") 
    }
    "the success of an example" in {
      there was one(notifier).exampleSucceeded("ex1-1") 
    }
    "the failure of an example" in {
      there was one(notifier).exampleFailed("ex1-2", new FailureException("wrong"))
    }
    "the error of an example" in {
      there was one(notifier).exampleError(is_==("ex2-2"), is_==("bad") ^^ ((e:Throwable) => e.getMessage))
    }
    "a skipped example" in {
      there was one(notifier).exampleSkipped("ex2-3")
    }
    "the end of a system" in {
      there was one(notifier).systemCompleted("system1 should")
    }
  }
  "A notifier for a specification with an anonymous spec" should { 
    new NotifierRunner(specWithAnAnonymousSpec, notifier).reportSpecs
    "not be notified of the anonymous sus" in {
      there was no(notifier).systemStarting(specWithAnAnonymousSpec.systems(0).header)
    }
  }
  "A notifier for a planOnly specification" should beNotifiedOf { 
    "only the systems and examples when a specification" in {
      s.planOnly(true)
      new NotifierRunner(s, notifier).reportSpecs
      there was no(notifier).exampleSucceeded("ex1-1")
    }
    "only the systems and examples even of a subspecification" in {
      object s extends Specification {
        object included extends Specification {
          "ex1" in { 1 must_== 1 }
        }
        setPlanOnly()
        include(included)
      }
      new NotifierRunner(s, notifier).reportSpecs
      there was no(notifier).exampleSucceeded("ex1")
    }
  }
  val specWithAnAnonymousSpec = new Specification {
    "ex1" in { 1 must_== 1 }
    "ex2" in { 1 must_== 1 }
  }
  val s = new Specification {
    "system1"  should {
      "ex1-1" in { 1 must_== 1 }
      "ex1-2" in { fail("wrong") }
      fail("sus failed"); ()
    }
    "system2"  should {
      "ex2-1" in { 1 must_== 1 }
      "ex2-2" in { throw new Exception("bad") }
      "ex2-3" in { skip("skip this one") }
    }
    "system3"  should {
      error("sus error"); ()
    }
  }
  def beNotifiedOf = addToSusVerb(" be notified of ")
}
