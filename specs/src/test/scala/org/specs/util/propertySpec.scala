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
package org.specs.util
import org.specs.execute._

class propertySpec extends org.spex.Specification {
  "A property" should {
    "not be evaluate its update value until it is queried" in  {
      val p = Property(1)
      p({fail("must not be thrown"); 2}) must not (throwA[FailureException])
      p() must throwA[FailureException]
    }
    "allow its value to be modified directly" in {
      val p = new Property(() => Some(new scala.collection.mutable.ListBuffer[Int]))
      p.get.append(1)
      p.get.toList must_== List(1)
    }
    "allow its value to be updated" in {
      val p = Property(1)
      p.withValue(2).get must_== 2
    }
    "behave like an Option" in {
      "have an iterator method" >> {
        Property(1).iterator.toList must_== List(1)
      }
      "have a isDefined method" in {
        Property(1).isDefined must beTrue
        Property[Int]().isDefined must beFalse
      }
      "have a isEmpty method" in {
        Property(1).isEmpty must beFalse
        Property[Int]().isEmpty must beTrue
      }
      "have a filter method" in {
        Property(1).filter(_ > 0).isDefined must beTrue
        Property(1).filter(_ < 0).isDefined must beFalse
      }
      "have a flatMap method" in {
        Property(1).flatMap((i:Int) => Some(i)).get must_== 1
      }
      "have a foreach method" in {
        var i = 0
        Property(1).foreach(i += _)
        i must_== 1
      }
      "have a getOrElse method" in {
        Property(1).getOrElse(0) must_== 1
        Property[Int]().getOrElse(0) must_== 0
      } 
      "have a map method" in {
        Property(1).map(_.toString).get must_== "1"
      }
    }
  }
  "A reinitializable property" should { 
    "return its initial value when reinitialized" in {
      val p = ReinitProperty(0)
      p() must_== 0
      p(1).apply() must_== 1
      p.reinit.apply() must_== 0
    }
  }
}
