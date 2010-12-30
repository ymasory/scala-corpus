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

import org.specs._
import org.specs.specification._
import org.specs.collection.ExtendedIterable._
import org.specs.util.ExtendedThrowable._
import org.specs.util.Duration
import org.specs.matcher.MatcherUtils._
import org.specs.execute._
import scala.xml.Node
/**
 * <p>The <code>Matchers</code> object allows to import the functions from the <code>Matchers</code> trait. 
 */
object Matchers extends Matchers
/**
 * <p>The <code>Matchers</code> trait provides all existing Matchers to the 
 * <code>Specification</code> trait</p> 
 * They can be used with be/have + matcher syntax
 */
trait Matchers extends AnyMatchers with 
                       LogicalMatchers with
                       StringMatchers with
                       IterableMatchers with
                       MapMatchers with
                       NumericMatchers with
                       EitherMatchers with
                       PatternMatchers with 
                       XmlMatchers with 
                       FileMatchers with 
                       MatcherResult with 
                       EventuallyMatchers
/**
 * <p>The <code>BaseMatchers</code> trait provides all existing Matchers to the 
 * <code>Specification</code> trait</p> 
 * They can't be used with be/have + matcher syntax
 */
trait BaseMatchers extends AnyBaseMatchers with 
                           LogicalMatchers with
                           StringBaseMatchers with
                           IterableBaseMatchers with
                           MapBaseMatchers with
                           NumericBaseMatchers with
                           EitherBaseMatchers with
                           PatternBaseMatchers with 
                           XmlBaseMatchers with 
                           FileBaseMatchers with 
                           MatcherResult with 
                           EventuallyMatchers
                       
/**
 * <p>The <code>AbstractMatcher</code> class is the base class for Matchers.
 * 
 * This class should be subclassed to provide an appropriate <code>apply</code> method that 
 * will check a value <code>a</code></p>.
 * 
 * Children of that class can use the optional description string to provide enhanced failure messages.
 * 
 * <i>Implementation notes</i>:<ul>
 * <li>the parameter to the apply method should be a by-name parameter. This allow some parameters not to be evaluated
 * when not necessary. For example in <code>a must (m1(b) and m2(c))</code> m2(c) will not be evaluated if m1(b) is false</li>
 * <li>However in the implementation of the apply function, it must be taken care of not evaluating the parameter twice. Assigning it to 
 * a val is the solution to this issue.</li>
 * <li>2 messages are included in the result of the apply function, to allow the easy creation of the negation of matchers
 *  with the not method.</li>
 * <li>The description attribute and the 'd' method can be used to construct precise messages describing the
 * value being matched against</li>
 * </ul>
 * 
 */
abstract class AbstractMatcher[-T] {
  /** 
   * this function must be implemented by subclasses.
   * @return a triple with a boolean indicating the success or failure of the matcher and 2 messages, the ok message, 
   *  and the ko message
   */
  def apply(a: => T): (Boolean, String, String)

  /** 
   * Optional description of the expectable object that will be passed to the matcher.
   * It will be set on the matcher when creating it through the following construction:
   * a aka "this value" must beTrue.<br/>
   * The 'aka' ("also known as") method sets the description in the ExpectableFactory.
   */
  protected var desc: Option[String] = None

  /** @return the precise description of the example */
  def description = desc
  /** set the description of the matcher */
  def setDescription(d: Option[String]): this.type = { desc = d; this }

  /** @return the description of the matched value, quoted. */
  protected def d(value: Any) = description match {
    case None => q(value)
    case Some(desc: String) => desc + " " + q(value)
  }
  /** @return the description of the matched value, unquoted. */
  protected def dUnquoted(value: Any) = description match {
    case None => unq(value)
    case Some(desc) => desc + " " + unq(value)  
  }
}

/**
 * <p>This class is the base class for checking if a given value, of type T, is matching an expected value</p>
 * <p>The <code>matcher</code> parameter is a function which takes a value and return a Boolean saying if the match is ok and
 * 2 messages: 1 if the match is ok, and another one if the match is ko. Those messages should usually specify which was the 
 * expected value and which was the actual one</p>
 * <p> It is also possible to use the boolean logical operator on matchers: and, or, not, xor to combine matchers together. 
 * This is the essential reason why the ok message is included in the <code>matcher</code> function. For instance, when the 
 * <code>not</code> operator is used, the ok message is used as a ko message</p>
 *   
 */
abstract class Matcher[-T] extends AbstractMatcher[T] with MatcherResult { outer =>
  
  /**
   *  The <code>and</code> operator allow to combine to matchers through a logical and.
   *  <code>m1 and m2</code> can successfully match a value <code>a</code> only if m1 succeeds 
   *  and m2 succeeds also
   */   
  def and[U <: T](m: => Matcher[U]): Matcher[U] = { 
    new Matcher[U](){
      def apply(a: => U) = {
        val value = a
        outer.setDescription(this.description)
        val r1 = outer(value)
        if (!r1.success)
          (false, r1.okMessage, r1.koMessage)
         else {
           val andMatcher = m
           andMatcher.setDescription(this.description)
           val r2 = andMatcher(value) 
          (r2.success, r1.okMessage + " and " + r2.okMessage, r1.okMessage + " but " + r2.koMessage) 
        }
  }}}

  /**
   *  The <code>or</code> operator allow to combine to matchers through a logical or.
   *  <code>m1 or m2</code> can successfully match a value <code>a</code> if m1 succeeds 
   *  or m2 succeeds. <code>m2</code> is not evaluated if m1 succeeds
   */   
  def or[U <: T](m: => Matcher[U]) : Matcher[U] = { 
    new Matcher[U]() {
    def apply(a: =>U) = {
      val value = a
      outer.setDescription(this.description)
      val r1 = outer(value)
      if (r1.success)
        (true, r1.okMessage, r1.koMessage)
      else {
        val orMatcher = m
        orMatcher.setDescription(this.description)
        val r2 = orMatcher(value)
        if (r2.success)
          (true, r2.okMessage + " but " + r1.koMessage, r1.koMessage + " and " + r2.koMessage)
        else
          (false, r1.okMessage + " and " + r2.okMessage, r1.koMessage + " and " + r2.koMessage)
      }
  }}}

  /**
   *  The <code>xor</code> operator allow to combine to matchers through a logical xor.
   *  <code>m1 xor m2</code> can successfully match a value <code>a</code> if m1 succeeds 
   *  and m2 fails, or if m1 fails and m2 succeeds
   */   
  def xor[U <: T](m: => Matcher[U]) : Matcher[U] = (this and m.not) or (this.not and m)

  /**
   *  The <code>not</code> operator allow to combine to matchers through a logical not.
   *  <code>m1.not</code> returns a matcher failing if m1 succeeds, and succeeding if m1 fails
   */   
  def not = {       
    new Matcher[T]() {
    def apply(a: => T) = {
      outer.setDescription(this.description)
      val result = outer(a)
      (!result.success, result.koMessage, result.okMessage)
    }}
  }

  /**
   *  The <code>when</code> operator returns a matcher which will be ok only if a condition is true
   */   
  def when(condition : => Boolean) = { 
    new Matcher[T]() {
      outer.setDescription(this.description)
      def apply(a: => T) = {
          val result = outer(a)
          (if (condition) result.success else true, result.okMessage, result.koMessage)
      }}
  }
  /**
   *  The <code>unless</code> operator returns a matcher which will be ok only if a condition is false
   */   
  def unless(condition : => Boolean) = when(!condition)
  
  /**
   *  The <code>lazily</code> operator returns a matcher which will match a function returning the expected value
   */   
  def lazily = { 
    new Matcher[() => T]() {
     outer.setDescription(this.description)
     def apply(a: => (() => T)) = outer(a.apply)
    }
  }

  /**
   *  The <code>^^</code> operator returns a matcher which will apply a function before doing the match
   */   
  def composeWithFunction[A](f: A => T) = this ^^ f
  def ^^[A](f: A => T) = {
    new Matcher[A]() {
      outer.setDescription(this.description)
      def apply(a: => A) = outer(f(a))
    }
  }  

  /**
   *  The <code>orSkipExample</code> operator throws a SkippedException if the matcher fails
   */   
  def orSkipExample = { 
    val outer = this;
    new Matcher[T]() {
      outer.setDescription(this.description)
      def apply(a: => T) = {
          val result = outer(a)
          if (!result.success) {
            val skippedException = new SkippedException("skipped because " + result.koMessage)
            skippedException.throwWithStackTraceOf(new SkippedException("").removeTracesWhileNameMatches("(Expectable.scala|Matchers.scala)"))
          }
          (result.success, result.okMessage, result.koMessage)
      }}
  }

  /**
   *  Alias for orSkipExample
   */   
  def orSkip = orSkipExample
  
  /**
   * @return a matcher that needs to eventually match, after 40 retries and a sleep time of 100 milliseconds
   */
  def eventually: Matcher[T] = EventuallyMatchers.eventually(this)
  /**
   * @return a matcher that needs to eventually match, after a given number of retries and a sleep time
   */
  def eventually(retries: Int, sleep: Duration): Matcher[T] = EventuallyMatchers.eventually(retries, sleep)(this)
  /** 
   * @return a matcher that matches all elements of an iterable
   */
  def toIterable = new Matcher[Iterable[T]]() {
    type Result = (Boolean, String, String)
    
    def apply(v: =>Iterable[T]) = ((true, "", "") /: v) { (res: Result, cur: T) =>
      val currentRes = outer(cur)
      if (currentRes._1) 
        (res._1 && currentRes._1, append(res._2, " and ", currentRes._2),  append(res._3, " and ", currentRes._2))
      else
        (res._1 && currentRes._1, append(res._2, " and ", currentRes._2),  append(res._2, " but ", currentRes._3))
    }
  }
  private def append(first: String, separator: String, second: String) = 
      first + (if (first.isEmpty) "" else separator) + second
}
/**
 * Result of <code>Matcher.apply</code>. Provides a method named 'success' to get the result
 * of a match, as well as 'okMessage' and 'koMessage' to get the status messages.
 */   
trait MatcherResult {
  /**
   * This case class and the associated implicit definition are used to add more meaningful names to
   * the tuple representing the result of a match when implementing <code>Matcher</code> logical operators.<br/>
   * Usage: <code>matcher.apply(value).okMessage</code> for instance
   */  
  case class MatcherResult(success: Boolean, okMessage: String, koMessage: String) 
  implicit def toMatcherResult(t: (Boolean, String, String)): MatcherResult = MatcherResult(t._1, t._2, t._3)  
  implicit def toTuple(m: MatcherResult): (Boolean, String, String) = (m.success, m.okMessage, m.koMessage)

}
abstract class OkWordMatcher[T] extends Matcher[T]
class NotMatcher[T] extends Matcher[T] { 
  def apply(v: =>T) = (false, "", "")
}
class BeVerbMatcher[T] extends OkWordMatcher[T] { 
  def apply(v: =>T) = (true, "", "")
  def ==/(node: Seq[Node]) = new EqualIgnoringSpaceMatcher(node)
  def ==/(s: String) = new BeEqualToIgnoringCase(s)
  def !=/(s: String) = new BeEqualToIgnoringCase(s).not
  def <[T](n: T)(implicit d: T => Double, e: T => Ordered[T]) = new BeLessThan(n) 
  def <=[T](n: T)(implicit d: T => Double, e: T => Ordered[T]) = new BeLessThanOrEqualTo(n) 
  def >[T](n: T)(implicit d: T => Double, e: T => Ordered[T]) = new BeLessThanOrEqualTo(n).not
  def >=[T](n: T)(implicit d: T => Double, e: T => Ordered[T]) = new BeLessThan(n).not 
  def ~[T](n: T, delta: T)(implicit d: T => Monoid[T], e: T => Ordered[T]) = new BeCloseTo(n, delta)
}
class HaveVerbMatcher[T] extends OkWordMatcher[T] { 
  def apply(v: =>T) = (true, "", "")
  def \\(node: Node) = XmlBaseMatchers.\\(node)
  def \\(label: String) = XmlBaseMatchers.\\(label)
  def \\(node: Node, attributes: List[String]) = XmlBaseMatchers.\\(node, attributes)
  def \\(label: String, attributes: List[String]) = XmlBaseMatchers.\\(label, attributes)
  def \\(node: Node, attributeValues: Map[String, String]) = XmlBaseMatchers.\\(node, attributeValues)
  def \\(label: String, attributeValues: Map[String, String]) = XmlBaseMatchers.\\(label, attributeValues)
  def \\(node: Node, attributeValues: Pair[String, String]*) = XmlBaseMatchers.\\(node, attributeValues:_*)
  def \\(label: String, attributeValues: Pair[String, String]*) = XmlBaseMatchers.\\(label, attributeValues:_*)
  def \(node: Node) = XmlBaseMatchers.\(node)
  def \(label: String) = XmlBaseMatchers.\(label)
  def \(node: Node, attributes: List[String]) = XmlBaseMatchers.\(node, attributes)
  def \(label: String, attributes: List[String]) = XmlBaseMatchers.\(label, attributes)
  def \(node: Node, attributeValues: Map[String, String]) = XmlBaseMatchers.\(node, attributeValues)
  def \(label: String, attributeValues: Map[String, String]) = XmlBaseMatchers.\(label, attributeValues)
  def \(node: Node, attributeValues: Pair[String, String]*) = XmlBaseMatchers.\(node, attributeValues:_*)
  def \(label: String, attributeValues: Pair[String, String]*) = XmlBaseMatchers.\(label, attributeValues:_*)
}
class ArticleMatcher[T] extends OkWordMatcher[T] { 
  def apply(v: =>T) = (true, "", "")
}


