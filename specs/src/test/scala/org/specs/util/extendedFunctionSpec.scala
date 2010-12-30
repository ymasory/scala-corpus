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
import org.specs._
import ExtendedFunctions._

class extendedFunctionSpec extends SpecificationWithJUnit {
  "a function defined as a list of cases can have an applySafely method" >> {
	val f: PartialFunction[Int, String] = { case a if a > 0 => a.toString }
	"returning Some(value) if there's no MatchError" in {
	  f.applySafely(1) must beSome("1")
	}  
	"returning None in case of a MatchError" in {
	  f.applySafely(0) must be none;
	}  
	"rethrowing a match error if it isn't part of the function definition" in {
      val f2: Function[Int, String] = { case a => a match { case x if x > 0 => x.toString } }
	  f2.apply(-1) must throwA[MatchError]
	}  
  }
  "a method can accept a function or a partial function" >> {
	class Test[T](val t: T) {
     def sub[S](x:Function[T, S]): Option[S] = x.applySafely(t)
    }
    val test = new Test("hello")
    test sub identity must beSome("hello")
    test sub { case s if s.size < 2 => "partial " + s } must beNone
  }
}