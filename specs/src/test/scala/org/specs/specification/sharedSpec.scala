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
import org.specs.io.mock._

class sharedSpec extends SpecificationWithJUnit {
  "The specification with shared subexamples" should {
    "have not any pending results" in {
      (new MockJavaSpecification).reportSpecs.messages must not containMatch("PENDING")
    }
    "have one ko nested example" in {
      (new MockJavaSpecification).reportSpecs.messages must containMatch("x must be ko")
    }
    "have another ko example at a second level" in {
      (new MockJavaSpecification).reportSpecs.messages must containMatch("x must also be ko")
    }
    "have examples appear once only with a val definition" in {
      (new MockJavaSpecification2).reportSpecs.messages.filter(_ contains "must be ok") must have size(1)
    }
    "have no pending results when there are no nested examples" in {
      (new MockJavaSpecification3).reportSpecs.messages must not containMatch("PENDING")
    }
  }
}
class SharedExamples extends Specification {
  def shared = "The Scala language" should {
    "a nested example" >> { 
      "must be ok" >> {
        true must beTrue 
      } 
      "must be ko" >> {
        false must beTrue 
      } 
    }
    "another nested example" >> {
      "must also be ok" >> { 
        true must beTrue 
      } 
      "must also be ko" >> { 
        false must beTrue 
      } 
    }
  }
  shared
}
class JavaSpecification extends Specification {
  "The Java language" should {
    behave like (new SharedExamples).shared
  }
}
class SharedExamples2 extends Specification {
  val shared = "The Scala language" should {
    "a nested example" >> { 
      "must be ok" >> {
        true must beTrue 
      } 
    }
    "another nested example" >> {
      "must also be ok" >> {
        true must beTrue 
      } 
      "must also be ko" >> {
        false must beTrue 
      } 
    }
  }
}
class MockJavaSpecification extends Specification with MockOutput {
  "The Java language" should {
    behave like (new SharedExamples).shared
  }
}
class MockJavaSpecification2 extends Specification with MockOutput {
  "The Java language" should {
    behave like (new SharedExamples2).shared
  }
}
class JavaSpecification2 extends Specification {
  "The Java language" should {
    behave like (new SharedExamples2).shared
  }
}
class SharedExamples3 extends Specification {
  val shared = "The Scala language" should {
    "an example" >> { 
      true must beTrue 
    }
    "another example" >> {
      false must beTrue 
    }
  }
}
class MockJavaSpecification3 extends Specification with MockOutput {
  "The Java language" should {
    behave like (new SharedExamples3).shared
  }
}
class JavaSpecification3 extends Specification {
  "The Java language" should {
    behave like (new SharedExamples3).shared
  }
}
class JavaSpecification4 extends Specification {
  val shared = "The Scala language" should {
    "an example" >> { 
      true must beTrue 
    }
  }
  "The Java language" should {
    behave like shared
  }
}
