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
import org.specs.util.Products
import org.specs.io._
/**
 * Synctactic sugar for specifications. Since it makes heavy uses of implicit definitions,<br>
 * The name reminds that it must be used with caution
 */
object Sugar extends Sugar

/**
 * Synctactic sugar for specifications. Since it makes heavy uses of implicit definitions,<br>
 * The name reminds that it must be used with caution
 */
trait Sugar extends Products with ConsoleOutput with RepeatedActions { outer =>
  
  /** alias for the value true. Allows to write <code> myObject.status mustBe ok </code>*/
  val ok = true
  
  /** alias for the value false. Allows to write <code> myObject.status mustBe ko </code>*/
  val ko = false

  /** 
   * This implicit definition allows to print any object to the console with:<br/>
   * <code>myObject.pln</code> or <code>myObject.println</code>  
   */
  implicit def anyPrintable[T](a: T) = new Printable(a)
  class Printable[T](a: T){
    def println = outer.println(a)
    def pln = println

    /** print and pass: print the value and return it */ 
    def pp = { outer.println(a); a }
      
    /** alias for print and pass */ 
    def >| = pp
  }
  /** 
   * This implicit definition allows to print any iterable to the console with:<br/>
   * <code>myIterable.printEach</code>. It also returns the iterable for better method insertion  
   */
  implicit def iterableToPrintable[T](a: Iterable[T]) = new PrintableIterable(a)
  class PrintableIterable[T](a: Iterable[T]){
      /** print each element and return the iterable */ 
      def printEach = { a foreach(outer.println(_)); a }
      /** print each element forcing a given output object */ 
      def printEach(output: { def println(x: Any) }) = { a foreach(output.println(_)); a }
  }
}
trait NumberOfTimes {
  /** 
   * This implicit definition allows to declare a number of times
   * <code>3.times</code>
   */
  implicit def integerToRange(n: Int): RangeInt = new RangeInt(n)
  case class RangeInt(n: Int) { 
    def times = this 
  }
}
trait RepeatedActions {
  /** 
   * This implicit definition allows to write short loops, ruby-style:<br/>
   * <code> 3.times { doThis() } </code>. 
   */
  implicit def integerToRepeatedAction(n: Int) = new RepeatedAction(n)
  case class RepeatedAction(n: Int) { 
    def times[T](f: (Int) => T)  = for (i <- 1 to n) f(i) 
    def times(f: => Unit)  = for (i <- 1 to n) f 
  }
}
object RepeatedActions extends RepeatedActions
