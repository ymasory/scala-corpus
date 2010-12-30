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
import org.specs.Specification
import org.specs.runner._
import org.specs._

class specsFilterSpec extends SpecificationWithJUnit {
  "a specs filter" should {
    "filter the SUS of the specification according to a regular expression" in {
      object spec extends Specification {
        "this sus is sus1 and it" should { "have one example" in {} }
        "this sus is sus2 and it" should { "have one example" in {} }
      }
      val systems = filter(List(spec), ".*sus2.*", ".*").head.systems
      systems.size must_== 1
      systems.head.description must beMatching("sus2")
    }
    "filter the examples of the specification according to a regular expression" in {
      object spec extends Specification {
        "this sus is sus1 and it" should {
          "have one example ex1" in {}
          "have one example ex2" in {}
        }
        "this sus is sus2 and it" should { "have one example ex1" in {} }
      }
      val systems = filter(List(spec), ".*", ".*ex1").head.systems
      systems.size must_== 2
      val s1 = systems(0)
      s1.examples.size must_== 1
      s1.examples.head.description must beMatching("ex1")

      val s2 = systems(1)
      s2.examples.size must_== 1
      s2.examples.head.description must beMatching("ex1")
    }
    "check the patterns for sus and examples" in {
      val filter = new SpecsFilter {
        val specs = List(new Specification {})
        override def susFilterPattern = "[][]BAD PATTERN"
        override def exampleFilterPattern = "[][]BAD PATTERN"
      }
      val throwASpecsFilterPatternException: ExceptionClassMatcher[_] = throwA[SpecsFilterPatternException]
      filter.susFilter must throwASpecsFilterPatternException.like {
        case e: Exception => e.getMessage contains "Wrong pattern for the sus filter: Unclosed character class near index 14"
      }
      filter.exampleFilter must throwASpecsFilterPatternException.like {
        case e: Exception => e.getMessage contains "Wrong pattern for the example filter: Unclosed character class near index 14"
      }
    }
  }
  def filter(specifications: List[Specification], susToFilter: String, examplesToFilter: String) =  new SpecsFilter {
    val specs = specifications
    override def susFilterPattern = susToFilter
    override def exampleFilterPattern = examplesToFilter
  }.filter(specifications)
}
