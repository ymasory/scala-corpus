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
import org.specs.util.Configuration
import org.specs.runner._
import org.specs.Specification

class specificationExecutorSpec extends org.spex.Specification {
  "An executed specification, with one spec instance per example" should {
    "execute examples only once" in {
      specWithCountedExamples.failures // execute the specification
      examplesExecutionCounter.nb must_== 2
    }
    "mention the right number of expectations" in {
      specificationWithASharedVariable.failures // execute the specification
      val example = specificationWithASharedVariable.examples(0)
      example.expectationsNb must_== 1
    }
    "execute all subexamples" in {
      specificationWithSubexamples.allExamples must have size(3)
      specificationWithSubexamples.failures must have size(1)
    }
  }
  include(specificationWithASharedVariable, 
          specificationWithChangedConfiguration,
          specificationWithMockito,
          specificationWithANestedSpecification,
          specificationWithANestedCaseClassSpecification,
          specificationWithSetSequential,
          sequentialSpecWithNotifier,
          specWithSeparateContexts)
  
  "A specification for issue 102" should {
    "not skip an example when run with the NotifierRunner" in {
      notifiedSpecificationWithJMock.reportSpecs
      testNotifier.skippedExample aka "the expectation is skipped" must beFalse
    }
  }
}
object specWithCountedExamples extends org.spex.Specification {
  "first ex" in {
    1 must_== 1
    examplesExecutionCounter.nb += 1
  }
  "second ex" in {
    1 must_== 1
    examplesExecutionCounter.nb += 1
  }
}
object specificationWithASharedVariable extends org.spex.Specification {
  var i = 0
  "When executing each example, a shared variable" should {
    "be set to its initial value: 0" in { i must_== 0; i = i + 1 }
    "still be set to its initial value: 0" in { i must_== 0 }
    "be possibly used in subexamples" in {
      "here" in { i must_== 0 }
      "there" in { i must_== 0 }
    }
  }
}
object examplesExecutionCounter {
  var nb = 0
}
object specificationWithChangedConfiguration extends org.spex.Specification {
  shareVariables()
  var i = 0
  "When executing each example with shareVariables(), a shared variable" should {
    "be set to its initial value: 0" in { i must_== 0; i = i + 1 }
    "be incremented by the first example" in { i must_== 1 }
  }
}
object specificationWithMockito extends org.spex.Specification {
  var l = mock[java.util.List[String]]
  "When using the Mockito trait" should {
    "mocks should be setup ok" in { 
      l.get(0) returns "hello"
      l.get(0) must_== "hello"
    }
  }
}
object specificationWithANestedSpecification extends org.spex.Specification {
  "When executing a specification with a nested spec, there" should {
    "be no instantiation issue" in { 0 must_== 0 }
  }
  object s1 extends org.spex.Specification {
    0 must_== 0
  }
  include(s1)
}
object specificationWithANestedCaseClassSpecification extends org.spex.Specification {
  "When executing a specification with a case spec, there" should {
    "be no instantiation issue" in { 0 must_== 0 }
  }
  case class caseClassSpecification() extends org.spex.Specification {
    "When executing a specification with a case spec, there" should {
      "be no instantiation issue" in { 0 must_== 0 }
    }
  }
  include(new caseClassSpecification)
}
object specificationWithSubexamples extends org.spex.Specification {
  "execute all subexamples" should {
    "ex" in {
      "subex1" in {
        1 must_== 1 
      }
      "subex2" in {
        1 must_== 0
      }
      "subex3" in {
        1 must_== 1 
      }
    }
  }
}
// from issue 102
object specificationWithExpectation extends Specification {
 "An example" should{
   "assert expectations in example" in {
     1.isExpectation
   }
 }
 "Another example " should{
   "be run in complete isolation" in{
     1 must_== 1
   }
 }
}
// from issue 105
object Watcher {
  var messages = ""
  var count = 0
  def reset = { messages = ""; count = 0 }
  def addMessage(m: String) = { messages += count + "-" + m + "\n"; count +=1 }
}
class sequentialSpecification extends Specification {
  setSequential()
  var x = 0 
  "Foo" should {
    Watcher.addMessage("define ex1")
    "not go to busyloop" in {
      Watcher.addMessage("ex1")
      Watcher.messages must include("0-define ex1")
      Watcher.messages must include("1-ex1")
      x must_== 0; x = x + 1
    }
    Watcher.addMessage("define ex2")
    "not go to busyloop2" in {
      Watcher.addMessage("ex2")
      Watcher.messages must include("2-define ex2")
      Watcher.messages must include("3-ex2")
      x aka "x twice" must_== 1; x = x + 1
    }
    Watcher.addMessage("define ex3")
    "have 3 examples" in {
      Watcher.addMessage("ex3")
      Watcher.messages must include("4-define ex3")
      Watcher.messages must include("5-ex3")
      x aka "x thrice" must_== 2
    }
  }
}
class notSequentialSpecification extends Specification {
  Watcher.reset
  setNotSequential()
  "Foo" should {
    var x = 0 
    Watcher.addMessage("define ex1")
    "not go to busyloop" in {
      Watcher.addMessage("ex1")
      x must_== 0; x = x + 1
    }
    Watcher.addMessage("define ex2")
    "not go to busyloop2" in {
      Watcher.messages must include("0-define ex1")
      Watcher.messages must include("1-define ex2")
      x aka "x twice" must_== 0
    }
  }
}
object specificationWithSetSequential extends Specification {
  "If the spec is sequential, the first example must be executed when defined and variables should be shared" in {
    (new sequentialSpecification).failures must be empty
  }
  "If the spec is not sequential, the 2 examples should be defined first, then executed and there should be no shared variable" in {
    (new notSequentialSpecification).failures must be empty
  }
}

// from issue 106
object sequentialSpecWithNotifier extends Specification {
  testNotifier.reset
  notifiedSequentialSpecification.reportSpecs
  "There must be side-effects" in { testNotifier.failures must be empty }
  "Examples must only be executed once" in { testNotifier.succeeded must_== 6 }
}
// from issue 107
object specWithSeparateContexts extends Specification {
  "The first example must not fail" in {
    testNotifier.reset
    new NotifierRunner(specWithContexts, testNotifier).reportSpecs
    testNotifier.failures must be empty
  }
}
object specWithContexts extends ContextDefinition {
  "sus" ->-(context1) should {
    "access context1" in {
      context must be("context1")
    }
  }
  "sus2" ->-(context1) should {
    "access context1" in{
      context must be("context1")
    }
  }
}
trait ContextDefinition extends Specification {
  var context = "1"
  val context1 = beforeContext { 
    context = "context1" 
  }
}
object notifiedSpecWithContexts extends NotifierRunner(specWithContexts, new ConsoleNotifier)
object notifiedSequentialSpecification extends NotifierRunner(new sequentialSpecification, testNotifier)
object notifiedSpecificationWithJMock extends NotifierRunner(specificationWithExpectation, testNotifier)
object testNotifier extends Notifier {
  var skippedExample = false
  var failures: List[String] = Nil
  var errors = 0
  var succeeded = 0
  def reset = failures = Nil; errors = 0; succeeded = 0
  def runStarting(examplesCount: Int) = ()
  def exampleStarting(exampleName: String)  = ()
  def exampleSucceeded(testName: String) = {succeeded += 1}
  def exampleCompleted(testName: String) = {}
  def exampleFailed(testName: String, e: Throwable) = failures = failures ::: List(e.getMessage)
  def exampleError(testName: String, e: Throwable) = errors += 1
  def exampleSkipped(testName: String) = skippedExample = true
  def systemStarting(systemName: String) = ()
  def systemSucceeded(testName: String) = ()
  def systemFailed(testName: String, e: Throwable) = ()
  def systemError(testName: String, e: Throwable) = ()
  def systemSkipped(testName: String) = skippedExample = true
  def systemCompleted(systemName: String) = ()
}
