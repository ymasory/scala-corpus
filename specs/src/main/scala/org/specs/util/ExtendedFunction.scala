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

object ExtendedFunctions extends ExtendedFunctions 
trait ExtendedFunctions {
  implicit def extend[A, B](f: Function[A, B]) = new ExtendedFunction(f)
}
/**
 * this class allows to use a function as if it was a partial function.
 *
 * It is especially useful when one want to overload a function with both functions and partial function
 * literals. This usually is an issue because of the way the compiler interprets function and 
 * partial function literals (see the Scala Language Specification, section 8.5). 
 *
 * The following specification shows the how to use it: 
 * <code>
 * 	class Test[T](val t: T) {
 *     def call[S](x: Function[T, S]): Option[S] = x.applySafely(t)
 *  }
 *  val test = new Test("hello")
 *  test call identity must beSome("hello")
 *  // this behaves like a partial function but is a function really
 *  test call { case s if s.size < 2 => "partial " + s } must beNone
 * </code>
 * 
 */
class ExtendedFunction[A, B](f: Function[A, B]) {
  def applySafely(a: A): Option[B] = {
	try {
	  Some(f(a))
	} catch {
	  case e: MatchError => None
	}
  }
}