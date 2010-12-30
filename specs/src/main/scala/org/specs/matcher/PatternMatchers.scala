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
import org.specs.matcher.MatcherUtils._
import org.specs.specification.Result
/**
 * The <code>PatternMatchers</code> trait provides matchers which allow to use pattern matching
 * to match expressions.
 */
trait PatternMatchers extends PatternBaseMatchers with PatternBeHaveMatchers 
trait PatternBaseMatchers {

  /**
   * Matches if the value <code>v</code> is like the pattern <code> { case expression => boolean }</code><p>
   * It uses the fact that we can use the following syntax to map Options:<ul>
   *  <li><code> myOption.map { case expression => boolean }</code><p></ul>
   * In that case, the pattern parameter would be <code>{ case expression => boolean }</code>, a function of type <code>Any => Boolean</code><p>
   * The <code>Sugar</code> object can be used to get a shorter expression by having the <code>ok</code> alias for <code>true</code>:
   *  <ul><li> <code>List(1, 2) must beLike { case x::y::Nil => ok }</code></ul>
   *
   * @param pattern a case expression
   * @return false if there is no match
   * @return the value inside the mapped option if there is a match, which should be <code>true</code>
   */
  def beLike[T](pattern: => PartialFunction[T, Boolean]) = new Matcher[T]() {
    def apply(v: => T) = {
      val value = v
      (if (value == null || !pattern.isDefinedAt(value)) 
          false 
        else 
          pattern.apply(value)
       ,
       d(value) + " matches the given pattern",
       d(value) + " doesn't match the expected pattern")
    }
  }
  /**
   * Alias for beLike
   */
  def beLikeA[T](pattern: => PartialFunction[T, Boolean]) = beLike(pattern)

  /**
   * Matches if the value <code>v</code> is None
   */
  def beNone = new Matcher[Option[Any]]() {
    def apply(v: => Option[Any]) = {
      val value = v
      val none: Option[Any] = None
      (value match {
         case n if (n == none) => true
         case _ => false
       },
       d(value) + " is None",
       d(value) + " is not None")
    }
  }

  /**
   * @deprecated use beAsNoneAs
   */
  def beAlsoNone[T](a: =>Option[T]) = beAsNoneAs(a)
  /**
   * Matches if a is None when v is None and a is not None when v is not None
   */
  def beAsNoneAs[T](a: =>Option[T]) = new Matcher[Option[T]]() {
    def apply(v: =>Option[T]) = {
      val x = a;
      val y = v;
      (x == None && y == None || x != None && y != None, "both values are None",
       if (x == None) d(y) + " is not None" else d(x) + " is not None")
    }
  }

  /**
   * Matches if the value <code>v</code> is Some(x)
   */
  def beSome[T] = new CaseMatcher[T]() {
    def someApply(v: => Option[T]) = {
      val value = v
      (value match {
         case Some(x) => true
         case _ => false
       },
       d(value) + " is Some(x)",
       d(value) + " is not Some(x)")
    }
  }

  /**
   * Matches if the value <code>v</code> is Some(x)
   */
  def beSome[T](expected: T) = new CaseMatcher[T]() {
    def someApply(v: => Option[T]) = {
      val value = v
      (value match {
         case Some(`expected`) => true
         case _ => false
       },
       d(value) + " is " + q("Some("+expected+")"),
       d(value) + " is not " + q("Some("+expected+")"))
    }
  }

  /**
   * Alias for beSome[Any]
   */
  def beSomething = beSome[Any]

  /**
   * The CaseMatcher class allow to verify expressions such as:<br>
   * <code>Some(x) must beSome[String].which(_.startWith("abc"))</code>
   */
  abstract class CaseMatcher[T] extends Matcher[Option[T]] {
    private var whichFunction: Option[T => Boolean] = None
    def someApply(value: => Option[T]): (Boolean, String, String)

    def which(g: T => Boolean) = {
      whichFunction = Some(g)
      this
    }
    override def apply(a: => Option[T]) =
      if (whichFunction == Some(null))
        (false, "the 'which' property is a not a null function", "the 'which' property is a null function")
      else
        whichFunction match {
          case None => someApply(a)
          case Some(g) => ( a match {
                          case Some(x) => g(x)
                          case _ => false
                        },
                        description.getOrElse("there") + " is a Some(x) verifying the given property",
                        description.getOrElse("there") + " is no Some(x) verifying the given property")
       }
  }
}
trait PatternBeHaveMatchers { outer: PatternBaseMatchers =>
  /** 
   * matcher aliases and implicits to use with BeVerb and HaveVerb
   */
  implicit def toPatternResult[T](result: Result[T]) = new PatternResultMatcher(result)
  class PatternResultMatcher[T](result: Result[T]) {
    def like(pattern: => PartialFunction[T, Boolean]) = result.matchWithMatcher(beLike(pattern))
  }
  implicit def toOptionPatternResult[T](result: Result[Option[T]]) = new OptionResultMatcher(result)
  class OptionResultMatcher[T](result: Result[Option[T]]) {
    def asNoneAs(a: =>Option[T]) = result.matchWithMatcher(beAsNoneAs(a))
    def beNone = result.matchWithMatcher(outer.beNone)
    def none = result.matchWithMatcher(outer.beNone)
    def beSome = result.matchWithMatcher(outer.beSome)
    def some = result.matchWithMatcher(outer.beSome)
  }
  implicit def toSomePatternResult[T](result: Result[Some[T]]) = new SomeResultMatcher(result)
  class SomeResultMatcher[T](result: Result[Some[T]]) {
    def asNoneAs(a: =>Some[T]) = result.matchWithMatcher(beAsNoneAs(a))
    def beNone = result.matchWithMatcher(outer.beNone)
    def none = result.matchWithMatcher(outer.beNone)
    def beSome = result.matchWithMatcher(outer.beSome)
    def some = result.matchWithMatcher(outer.beSome)
  }
  def like[T](pattern: => PartialFunction[T, Boolean]) = beLike(pattern)
  def asNoneAs[T](a: =>Option[T]) = beAsNoneAs(a)
  def none = beNone
  def some = beSome
}
/**
 * Companion object for PatternMatchers.
 */
object PatternMatchers extends PatternMatchers
