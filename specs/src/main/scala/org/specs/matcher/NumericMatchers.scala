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
package org.specs.matcher
import org.specs.specification.Result
/**
 * The <code>NumericMatchers</code> trait provides matchers which allow numerical comparisons
 */
trait NumericMatchers extends NumericBaseMatchers with NumericBeHaveMatchers
trait NumericBaseMatchers {
  import NumericMatchersUtil._
  /**
   * Matches if x < n.
   */   
  def beLessThan[S <% Ordered[S]](n: S) = new BeLessThan(n)

  /**
   * Alias for beLessThan.
   */   
  def be_<[S <% Ordered[S]](n: S) = beLessThan(n)
  /**
   * Alias for beLessThan.
   */   
  def <[S <% Ordered[S]](n: S) = beLessThan(n)

  /**
   * Matches if x <= n.
   */   
  def beLessThanOrEqualTo[S <% Ordered[S]](n: S) = new BeLessThanOrEqualTo(n)
  /**
   * Alias for beLessThanOrEqualTo.
   */   
  def be_<=[S <% Ordered[S]](n: S) = beLessThanOrEqualTo(n)
  /**
   * Alias for beLessThanOrEqualTo.
   */   
  def <=[S <% Ordered[S]](n: S) = beLessThanOrEqualTo(n)
  /**
   * Matches if x >= n.
   */   
  def beGreaterThanOrEqualTo[S <% Ordered[S]](n: S) = beLessThan(n).not
  /**
   * Alias for beGreaterThanOrEqualTo.
   */   
  def be_>=[S <% Ordered[S]](n: S) = beGreaterThanOrEqualTo(n)
  /**
   * Alias for beGreaterThanOrEqualTo.
   */   
  def >=[S <% Ordered[S]](n: S) = beGreaterThanOrEqualTo(n)
  /**
   * Matches if x > n.
   */   
  def beGreaterThan[S <% Ordered[S]](n: S) = beLessThanOrEqualTo(n).not
  implicit def longToDouble(s: Long): scala.Double = java.lang.Double.parseDouble(s.toString)
  /**
   * Alias for beGreaterThan.
   */   
  def be_>[S <% Ordered[S]](n: S) = beGreaterThan(n)
  /**
   * Alias for beGreaterThan.
   */   
  def >[S <% Ordered[S]](n: S) = beGreaterThan(n)
  /**
   * Matches if x = n +/- delta.
   */   
  def beCloseTo[S](n: S, delta: S)(implicit d: S => Monoid[S], e: S => Ordered[S]) = new BeCloseTo(n, delta)
  /** implicit definition to create delta for the beCloseTo matcher */
  implicit def ToDelta[S](n: S) = CanHaveDelta(n)
  /** transient class allowing the creation of a delta */
  private[specs] case class CanHaveDelta[S](n: S) {
    def +/-(delta: S) = Delta(n, delta)
  }
  /** class representing a numeric range */
  private[specs] case class Delta[S](n: S, delta: S)
  /**
   * Matches if x = n +/- delta.
   */   
  def beCloseTo[S](delta: Delta[S])(implicit d: S => Monoid[S], e: S => Ordered[S]) = new BeCloseTo(delta.n, delta.delta)
  /**
   * Alias for beCloseTo.
   */   
  def ~[S](n: S)(delta: S)(implicit d: S => Monoid[S], e: S => Ordered[S]) = beCloseTo(n, delta)
}
object NumericMatchersUtil {
  /** format a number: 1 must be 1 and not 1.0 if it is an integer. */
  def f[D <% Ordered[D]](x: D): String = x.toString
}

trait NumericBeHaveMatchers { this: NumericBaseMatchers => 
  import NumericMatchersUtil._
  /** 
   * matcher aliases and implicits to use with BeVerb and HaveVerb 
   */
  implicit def toNumericalResultMatcher[S](result: Result[S])(implicit d: S => Monoid[S], e: S => Ordered[S]) = new NumericalResultMatcher(result)
  class NumericalResultMatcher[S](result: Result[S])(implicit d: S => Monoid[S], e: S => Ordered[S]) {
    def <(n: S) = result.matchWith(beLessThan(n)) 
    def <=(n: S) = result.matchWith(beLessThanOrEqualTo(n)) 
    def >(n: S) = result.matchWith(beGreaterThan(n))
    def >=(n: S) = result.matchWith(beGreaterThanOrEqualTo(n)) 
    def lessThan(n: S) = result.matchWith(beLessThan(n))
    def lessThanOrEqualTo(n: S) = result.matchWith(beLessThanOrEqualTo(n)) 
    def greaterThan(n: S) = result.matchWith(beGreaterThan(n))
    def greaterThanOrEqualTo(n: S) = result.matchWith(beGreaterThanOrEqualTo(n)) 
    def closeTo(n: S, delta: S) = result.matchWith(beCloseTo(n, delta))
    def closeTo(delta: Delta[S]) = result.matchWith(beCloseTo(delta))
    def ~(n: S, delta: S) = result.matchWith(beCloseTo(n, delta))
  }
  def lessThan[S <% Ordered[S]](n: S) = beLessThan(n) 
  def lessThanOrEqualTo[S <% Ordered[S]](n: S) = beLessThanOrEqualTo(n) 
  def greaterThan[S <% Ordered[S]](n: S) = beGreaterThan(n) 
  def greaterThanOrEqualTo[S <% Ordered[S]](n: S) = beGreaterThanOrEqualTo(n) 
  def closeTo[S](n: S, delta: S)(implicit d: S => Monoid[S], e: S => Ordered[S]) = beCloseTo(n, delta) 

  implicit def intToMonoid(v: Int) = new Monoid[Int] {
    def +(t: Int) = v + t
    def -(t: Int) = v - t
  }
  implicit def doubleToMonoid(v: Double) = new Monoid[Double] {
    def +(t: Double) = v + t
    def -(t: Double) = v - t
  }
  implicit def longToMonoid(v: Long) = new Monoid[Long] {
    def +(t: Long) = v + t
    def -(t: Long) = v - t
  }
  implicit def floatToMonoid(v: Float) = new Monoid[Float] {
    def +(t: Float) = v + t
    def -(t: Float) = v - t
  }
}
trait Monoid[T] {
  def +(t: T): T
  def -(t: T): T
}

import NumericMatchersUtil._
class BeLessThanOrEqualTo[S <% Ordered[S]](n: S) extends Matcher[S] { 
  def apply(v: => S) = {
      val x = v
      (x <= n, if (x < n) dUnquoted(f(x)) + " is less than " + f(n) else dUnquoted(f(x)) + " is equal to " + f(n), 
               dUnquoted(f(x)) + " is greater than " + f(n))
    }
}
class BeLessThan[S <% Ordered[S]](n: S) extends Matcher[S] { 
  def apply(v: => S) = {
    val x = v
    (x < n, dUnquoted(f(x)) + " is less than " + f(n), dUnquoted(f(x)) + " is not less than " + f(n))
  }
}
class BeCloseTo[S](n: S, delta: S)(implicit d: S => Monoid[S], e: S => Ordered[S]) extends Matcher[S] {
  def apply(v: => S) = {val x = v; ((d(n) - delta <= x) && (x <= d(n) + delta), 
                                      dUnquoted(f(x)) + " is close to " + f(n) + " +/- " + delta, 
                                      dUnquoted(f(x)) + " is not close to " + f(n) + " +/- " + delta)
  }
}