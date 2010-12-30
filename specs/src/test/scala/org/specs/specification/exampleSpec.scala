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
import org.specs._
import org.specs.execute._
import org.specs.runner._
import org.specs.util._

class exampleSpec extends SpecificationWithJUnit {
  "An example" should {
    "know if it has subexamples without executing them" in {
      object s extends Specification {
        "e" in {
          "e1" in { 
            "e2" in { 1.isExpectation } 
          }
        }
      }
      s.examples(0) aka "first example" must beTrue ^^ { e  => e.hasSubExamples }
      s.examples(0).examples(0) aka "first nested example" must beTrue ^^ { e  => e.hasSubExamples }
    }
    "not be executed if not asked for results" in {
      ex.hasBeenExecuted must beFalse
    }
    "be executed if asked for results" in {
      ex.failures
      ex.hasBeenExecuted must beTrue
    }
  }
  "An example" can {
    "be resetted for execution" in {
      ex.resetExample
      ex.hasBeenExecuted must beFalse
      ex.failures
      ex.hasBeenExecuted must beTrue
    }
  }
  "An example" should {
    "throw a SkippedException with a PENDING message if it has a body with no expectations" in {
      object s extends Specification { 
        shareVariables()
        "this is a pending example" in {}
      }
      s.skipped must_== List(new SkippedException("PENDING: not yet implemented"))
    }
    "not throw a SkippedException with a PENDING message if it has a body with no expectations and the configuration" +
    "has examplesWithoutExpectationsMustBePending=false" in {
      Configuration.config = new Configuration { override val examplesWithoutExpectationsMustBePending = false }
      object s extends Specification { 
        shareVariables()
        "this is a pending example" in {} 
      }
      s.skipped must be empty
    }
  }
}
object ex extends Specification {
  shareVariables()
  var hasBeenExecuted = false
  var subexample: Example = null
  val testExample = "ex" in {
    hasBeenExecuted = true
  }
  override def failures = testExample.failures.toList
  def resetExample = { hasBeenExecuted = false; testExample.resetForExecution }
}
