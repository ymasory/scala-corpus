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
import org.specs.runner._
import org.specs.specification._
import org.specs.util.DataTables
import org.specs.runner._

class calculatorSpec extends HtmlSpecificationWithJUnit("Calculator Specification") with DataTables {
  val calc = new Object { def add(x: Int, y: Int): Int = x + y }

"A literate specification for a calculator" ->> <wiki>

*Examples of calculations*

{"this is an example ok" in { 1+1 must_==2 } }

A calculator can add integers: {
  "calc.add(a, b) == c"  inTable
  "First operand" 	| "Second operand" | "Result" |
   1  !  2  !  3  |
   1  !  2  !  3  |
   2  !  2  !  4  |
   2  !  2  !  4  |
   2  !  2  !  4  |
   { (a,b,c) => c must_== calc.add(a, b) }
}

 Nice, isn't it?

</wiki>

  "A classical specification for a calculator" should {
    "use tables directly" in {
                     "a" 	| "b" | "c" |>
                      1	   	!  2  !  3  |
                      2     !  2  !  4  |
                      2     !  6  !  8  | {(a,b,c) => c must_== calc.add(a, b) }
   }
 }

}
