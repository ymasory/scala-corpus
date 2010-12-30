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
package org.specs.samples

class isolatedExamples2 extends org.specs.SpecificationWithJUnit { 
  println("root") 
  var x = 0 
  "it" should {
  x must_== 0 
  "a" in { 
    println("a") 
    x += 1 
    x must_== 1 
    "aa" in { 
      println("aa") 
      x += 1 
      x must_== 2 
      "aaa" in { 
        println("aaa") 
        x += 1 
        x must_== 3 
      } 
      "aab" in { 
        println("aab") 
        x += 1 
        x must_== 3 
      } 
    } 
    "ab" in { 
      println("ab") 
      x += 1 
      x must_== 2 
    } 
  } 
  "b" in { 
    println("b") 
    x += 1 
    x must_== 1 
    "ba" in { 
      println("ba") 
      x += 1 
      x must_== 2 
      "baa" in { 
        println("baa") 
        x += 1 
        x must_== 3 
      } 
      "bab" in { 
        println("bab") 
        x += 1 
        x must_== 3 
      } 
    } 
    "bb" in { 
      println("bb") 
      x += 1 
      x must_== 2 
    } 
  }
  }
} 