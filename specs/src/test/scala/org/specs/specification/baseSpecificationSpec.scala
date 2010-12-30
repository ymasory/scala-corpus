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
import org.specs.util.Property
import org.specs._

class baseSpecificationSpec extends org.spex.Specification {
  def threeSpecs = List(new Specification{}, new Specification{}, new Specification{})
  
  "Specifications" can {
    "not be included in each other way" in {
      val s1 :: s2 ::x = threeSpecs
      s1.include(s2)
      s2.include(s1)
      s1 contains s2 must beTrue
      s2 contains s1 aka "specs2 contains specs1" must beFalse
    }
    "not be included in a cycle chain" in {
      val s1 :: s2 :: s3 ::x = threeSpecs
      s1.include(s2)
      s2.include(s3)
      s3.include(s1)
      s1 contains s2 must beTrue
      s2 contains s3 must beTrue
      s3 contains s1 aka "specs3 contains specs1" must beFalse
    }
    "return the list of its parent specifications starting with the most immediate one" in {
      val s1 :: s2 :: s3 ::x = threeSpecs
      s1.include(s2)
      s2.include(s3)
      s3.parentSpecifications must_== List(s2, s1)
    }
    "share examples between sus using 'behave like'" in {
      object s extends Specification {
        var expectedContext = ""
        var currentContext = ""
        val c1 = beforeContext { currentContext = "c1" }
        val c2 = beforeContext { currentContext = "c2" }
        
        "the first sus"->-(c1) should { 
          "have one example using the context" in { 
            currentContext must be_==(expectedContext).when(expectedContext == 2) 
          } 
        }
        "the second sus"->-(c2) should {
          expectedContext = "c2"
          behave like "the first sus"
          "have one example using the context" in { currentContext must_== expectedContext } 
        }
        "the third sus"->-(c2) should {
          expectedContext = "c3"
          behave like "the first sus"
        }
      }
      s.systems(1).failures must be empty;
      s.systems(2).failures aka "a system with improper expectations" must be empty
    }
  }
}
