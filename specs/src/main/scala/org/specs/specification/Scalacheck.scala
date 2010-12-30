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
package org.specs
import org.specs.matcher.{ScalaCheckMatchers, ScalaCheckParameters}
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.Shrink
import org.scalacheck.Arbitrary
import org.specs.matcher.Parameters
import org.specs.specification._

/**
 * This trait can be mixed with a specification to allow the use of ScalaCheck in a specification
 */
trait ScalaCheck extends ScalaCheckMatchers with ScalaCheckParameters with ScalaCheckVerifications {
  this: ExpectableFactory with BaseSpecification =>
}

/**
 * This trait defines the "verify" operator which can be used to specify properties to check as examples.
 * Instead of writing:<pre><code>
 * "startsWith verifies (a + b).startsWith(a)" in {
 *    property((a: String, b: String) => (a + b).startsWith(a)) must pass
 * }
 * it is possible to write directly:
 *   
 * "startsWith" verifies ((a: String, b: String) => (a + b).startsWith(a))
 * 
 * </code>
 * </pre>
 * 
 * This will create an example named "startsWith" and check the corresponding property.
 * 
 * ScalaCheck parameters can be used with the "display" and set methods. For example:<pre><code>
 * 
 * // will display the results and stop testing when 150 are passing.
 * "startsWith" verifies ((a: String, b: String) => (a + b).startsWith(a)).display(minTestsOk->150) 
 * </code>
 * </pre>
 *  
 */
trait ScalaCheckVerifications { outer: ExpectableFactory with BaseSpecification with ScalaCheckParameters with ScalaCheckMatchers =>

  /** 
   * Transforms a function to an object supporting ScalaCheck parameters.
   * Any object can use this implicit definition. A stricter definition would
   * declare one implicit conversion per function arity from 1 to 6
   */
  implicit def anyToAnyWithParameters[T](f: T) = AnyWithParameters(f)

  /** 
   * Case class supporting the display and set functions to set-up ScalaCheck parameters
   * and pass them to the verifies functions below.
   */
  case class AnyWithParameters[T](function: T) {
    var params: Parameters = _
    def display(p: (Symbol, Int)*) =  { params = outer.display(p:_*); this}
    def display = { params = outer.display(); this }
    def set(p: (Symbol, Int)*) =  { params = outer.set(p:_*); this }
  }

  /** 
   * This implicit uses a string describing a function to check with ScalaCheck with the 
   * "verifies" function.
   */
  implicit def toVerifies(e: String) = VerifiableExpectation(e: String)

  /** 
   * Class supporting the verification of a function with ScalaCheck, up to 6 parameters.
   */
  case class VerifiableExpectation(e: String) {
     def verifies[A1](f: (A1) => Boolean)(implicit a1: Arbitrary[A1],
                                             s1: Shrink[A1]) = forExample(e) in { Prop.forAll(f) must pass }     
     def verifies[A1](f: AnyWithParameters[A1 => Boolean])(implicit 
                                                          a1: Arbitrary[A1],
                                                          s1: Shrink[A1]) = forExample(e) in { Prop.forAll(f.function) must pass(f.params) }
     
     def verifies[A1, A2](f: (A1, A2) => Boolean)(implicit 
                                                    a1: Arbitrary[A1], 
                                                    a2: Arbitrary[A2],
                                                    s1: Shrink[A1], 
                                                    s2: Shrink[A2]) = forExample(e) in { Prop.forAll(f) must pass }
     def verifies[A1, A2, P](f: AnyWithParameters[(A1, A2) => Boolean])(implicit 
                                                         p: P => Prop,
                                                         a1: Arbitrary[A1], 
                                                         a2: Arbitrary[A2],
                                                         s1: Shrink[A1], 
                                                         s2: Shrink[A2]) = forExample(e) in { Prop.forAll(f.function) must pass(f.params) }
     def verifies[A1, A2, A3, P](f: (A1, A2, A3) => Boolean)(implicit 
                                                          p: P => Prop,
                                                          a1: Arbitrary[A1], 
                                                          a2: Arbitrary[A2], 
                                                          a3: Arbitrary[A3],
                                                          s1: Shrink[A1], 
                                                          s2: Shrink[A2], 
                                                          s3: Shrink[A3]) = forExample(e) in { Prop.forAll(f) must pass }
     def verifies[A1, A2, A3, P](f: AnyWithParameters[(A1, A2, A3) => Boolean])(implicit 
                                                                               p: P => Prop,
                                                                               a1: Arbitrary[A1], 
                                                                               a2: Arbitrary[A2], 
                                                                               a3: Arbitrary[A3],
                                                                               s1: Shrink[A1], 
                                                                               s2: Shrink[A2],	 
                                                                               s3: Shrink[A3]) = forExample(e) in { Prop.forAll(f.function) must pass(f.params) }
     def verifies[A1, A2, A3, A4, P](f: (A1, A2, A3, A4) => Boolean)(implicit 
                                                          p: P => Prop,
                                                                  a1: Arbitrary[A1], 
                                                                  a2: Arbitrary[A2], 
                                                                  a3: Arbitrary[A3],
                                                                  a4: Arbitrary[A4],
                                                                  s1: Shrink[A1], 
                                                                  s2: Shrink[A2], 
                                                                  s3: Shrink[A3], 
                                                                  s4: Shrink[A4]) = forExample(e) in { Prop.forAll(f) must pass }
     def verifies[A1, A2, A3, A4, P](f: AnyWithParameters[(A1, A2, A3, A4) => Boolean])(implicit 
                                                                                     p: P => Prop,
                                                                                     a1: Arbitrary[A1], 
                                                                                     a2: Arbitrary[A2], 
                                                                                     a3: Arbitrary[A3],
                                                                                     a4: Arbitrary[A4],
                                                                                     s1: Shrink[A1], 
                                                                                     s2: Shrink[A2], 
                                                                                     s3: Shrink[A3], 
                                                                                     s4: Shrink[A4]) = forExample(e) in { Prop.forAll(f.function) must pass(f.params) }
     def verifies[A1, A2, A3, A4, A5, P](f: (A1, A2, A3, A4, A5) => Boolean)(implicit 
                                                             p: P => Prop,
                                                             a1: Arbitrary[A1], 
                                                             a2: Arbitrary[A2], 
                                                             a3: Arbitrary[A3],
                                                             a4: Arbitrary[A4],
                                                             a5: Arbitrary[A5],
                                                             s1: Shrink[A1], 
                                                             s2: Shrink[A2], 
                                                             s3: Shrink[A3], 
                                                             s4: Shrink[A4], 
                                                             s5: Shrink[A5]) = forExample(e) in { Prop.forAll(f) must pass }
     def verifies[A1, A2, A3, A4, A5, P](f: AnyWithParameters[(A1, A2, A3, A4, A5) => Boolean])(implicit 
                                                             p: P => Prop,
                                                             a1: Arbitrary[A1], 
                                                             a2: Arbitrary[A2], 
                                                             a3: Arbitrary[A3],
                                                             a4: Arbitrary[A4],
                                                             a5: Arbitrary[A5],
                                                             s1: Shrink[A1], 
                                                             s2: Shrink[A2], 
                                                             s3: Shrink[A3], 
                                                             s4: Shrink[A4], 
                                                             s5: Shrink[A5]) = forExample(e) in { Prop.forAll(f.function) must pass(f.params) }
     def verifies[A1, A2, A3, A4, A5, A6](f: (A1, A2, A3, A4, A5, A6) => Boolean)(implicit 
                                                          a1: Arbitrary[A1], 
                                                          a2: Arbitrary[A2], 
                                                          a3: Arbitrary[A3],
                                                          a4: Arbitrary[A4],
                                                          a5: Arbitrary[A5],
                                                          a6: Arbitrary[A6],
                                                          s1: Shrink[A1], 
                                                          s2: Shrink[A2], 
                                                          s3: Shrink[A3], 
                                                          s4: Shrink[A4], 
                                                          s5: Shrink[A5], 
                                                          s6: Shrink[A6]) = forExample(e) in { Prop.forAll(f) must pass }
     def verifies[A1, A2, A3, A4, A5, A6](f: AnyWithParameters[(A1, A2, A3, A4, A5, A6) => Boolean])(implicit 
                                                             a1: Arbitrary[A1], 
                                                             a2: Arbitrary[A2], 
                                                             a3: Arbitrary[A3],
                                                             a4: Arbitrary[A4],
                                                             a5: Arbitrary[A5],
                                                             a6: Arbitrary[A6],
                                                             s1: Shrink[A1], 
                                                             s2: Shrink[A2], 
                                                             s3: Shrink[A3], 
                                                             s4: Shrink[A4], 
                                                             s5: Shrink[A5], 
                                                             s6: Shrink[A6]) = forExample(e) in { Prop.forAll(f.function) must pass(f.params) }
     def verifies[A1, A2, A3, A4, A5, A6, A7, P](f: (A1, A2, A3, A4, A5, A6, A7) => Boolean)(implicit 
                                                          p: P => Prop,
                                                          a1: Arbitrary[A1], 
                                                          a2: Arbitrary[A2], 
                                                          a3: Arbitrary[A3],
                                                          a4: Arbitrary[A4],
                                                          a5: Arbitrary[A5],
                                                          a6: Arbitrary[A6],
                                                          a7: Arbitrary[A7],
                                                          s1: Shrink[A1], 
                                                          s2: Shrink[A2], 
                                                          s3: Shrink[A3], 
                                                          s4: Shrink[A4], 
                                                          s5: Shrink[A5], 
                                                          s6: Shrink[A6],
                                                          s7: Shrink[A7]) = forExample(e) in { Prop.forAll(f) must pass }
     def verifies[A1, A2, A3, A4, A5, A6, A7](f: AnyWithParameters[(A1, A2, A3, A4, A5, A6, A7) => Boolean])(implicit 
                                                             a1: Arbitrary[A1], 
                                                             a2: Arbitrary[A2], 
                                                             a3: Arbitrary[A3],
                                                             a4: Arbitrary[A4],
                                                             a5: Arbitrary[A5],
                                                             a6: Arbitrary[A6],
                                                             a7: Arbitrary[A7],
                                                             s1: Shrink[A1], 
                                                             s2: Shrink[A2], 
                                                             s3: Shrink[A3], 
                                                             s4: Shrink[A4], 
                                                             s5: Shrink[A5], 
                                                             s6: Shrink[A6],
                                                             s7: Shrink[A7]) = forExample(e) in { Prop.forAll(f.function) must pass(f.params) }
     def verifies[A1, A2, A3, A4, A5, A6, A7, A8](f: (A1, A2, A3, A4, A5, A6, A7, A8) => Boolean)(implicit 
                                                          a1: Arbitrary[A1], 
                                                          a2: Arbitrary[A2], 
                                                          a3: Arbitrary[A3],
                                                          a4: Arbitrary[A4],
                                                          a5: Arbitrary[A5],
                                                          a6: Arbitrary[A6],
                                                          a7: Arbitrary[A7],
                                                          a8: Arbitrary[A8],
                                                          s1: Shrink[A1], 
                                                          s2: Shrink[A2], 
                                                          s3: Shrink[A3], 
                                                          s4: Shrink[A4], 
                                                          s5: Shrink[A5], 
                                                          s6: Shrink[A6],
                                                          s7: Shrink[A7],
                                                          s8: Shrink[A8]) = forExample(e) in { theValue(Prop.forAll(f)).must(pass) }
     def verifies[A1, A2, A3, A4, A5, A6, A7, A8](f: AnyWithParameters[(A1, A2, A3, A4, A5, A6, A7, A8) => Boolean])(implicit 
                                                             a1: Arbitrary[A1], 
                                                             a2: Arbitrary[A2], 
                                                             a3: Arbitrary[A3],
                                                             a4: Arbitrary[A4],
                                                             a5: Arbitrary[A5],
                                                             a6: Arbitrary[A6],
                                                             a7: Arbitrary[A7],
                                                             a8: Arbitrary[A8],
                                                             s1: Shrink[A1], 
                                                             s2: Shrink[A2], 
                                                             s3: Shrink[A3], 
                                                             s4: Shrink[A4], 
                                                             s5: Shrink[A5], 
                                                             s6: Shrink[A6],
                                                             s7: Shrink[A7],
                                                             s8: Shrink[A8]) = forExample(e) in { theValue(Prop.forAll(f.function)).must(pass(f.params)) }
   }

}
