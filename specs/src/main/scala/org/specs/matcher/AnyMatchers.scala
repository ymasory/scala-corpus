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

import org.specs.specification._
import org.specs.matcher.MatcherUtils.{q, matches}
import org.specs.matcher.PatternMatchers._
import org.specs.util.ExtendedThrowable._
import org.specs.util.EditDistance._
import org.specs.util.Classes._
import org.specs.collection.ExtendedIterable._
import scala.collection.immutable.{Set => Removed}
import scala.collection.Set
import scala.reflect.ClassManifest
import org.specs.execute._

/**
 * The <code>AnyMatchers</code> object allows the import of matchers from the AnyMatchers trait.
 */
object AnyMatchers extends AnyMatchers
/**
 * The <code>AnyMatchers</code> trait provides matchers which are applicable to any scala reference or value
 * and can be used with a be/have + matcher syntax
 */
trait AnyMatchers extends AnyBaseMatchers with AnyBeHaveMatchers 
/**
 * The <code>AnyBaseMatchers</code> trait provides matchers which are applicable to any scala reference or value
 */
trait AnyBaseMatchers extends StructuralMatchers {
  /** adapt a matcher to another one having the expected type */
  implicit def adapt[T, S](m: Matcher[T])(implicit c: S => T): Matcher[S] = m ^^ c

  /**
   * Matches if (a eq b)
   */
  def be(a: =>Any) = new Matcher[Any]() {
    def apply(v: =>Any) = {
      val (x, y) = (a, v)
      (x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef], d(y) + " is the same as " + q(x), d(y) + " is not the same as " + q(x))}
  }
  /**
   * Matches if !(a eq b)
   */
  def notBe(a: =>Any) = be(a).not
  /**
   * Matches if (a == b)
   * @deprecated use beEqualTo instead
   */
  def beEqual[T](a: =>T) = beEqualTo(a)

  /**
   * Matches if (a == b)
   */
  def beEqualTo[T](a: =>T) = new BeEqualTo(a)

  /**
   * Matches if (a != b)
   * @deprecated use beDifferentFrom instead
   */
  def beDifferent[T](a: =>T) = beDifferentFrom(a)

  /**
   * Matches if (a != b)
   */
  def beDifferentFrom[T](a: =>T) = beEqualTo[T](a).not

  /**
   * Matches if (a == b)
   */
  def is_==(a: =>Any)(implicit details: Detailed) = new Matcher[Any](){
    def apply(v: =>Any) = {
      val (x, y) = (a, v)
      var dy = d(y)
      var qx = q(x)
   	  var differentTypesWarning = ""
      if (dy == qx) {
	    val xClass = getClassName(x)
	    val yClass = getClassName(y)
	    if (xClass != yClass) {
          dy = dy + ": " + yClass
          qx = qx + ": " + xClass
	    } else differentTypesWarning = ". Values have the same string representation but possibly different types like List[Int] and List[String]"
      }
      import org.specs.util.Products._
      def isNull[T](a: T) = null == a

      val failureMessage = details match {
        case full: fullDetails if (!isNull(x) && full.startDiffSize <= x.toString.size) => {
          EditMatrix(dy, qx).showDistance(full.separators, full.shortenSize).toList.mkString(" is not equal to ")
        }
        case _ => dy + " is not equal to " + qx + differentTypesWarning
      }
      ((x == y), dy + " is equal to " + qx, failureMessage)
    }
  }
  /**
   * Alias of is_==
   */
  def be_==(a: =>Any)(implicit d: Detailed) = is_==(a)(d)

  /**
   * Matches if (a neq b)
   */
  def notEq(a: =>Any) = be(a).not

  /**
   * Matches if (a != b)
   */
  def is_!=(a: =>Any)(implicit d: Detailed) = (is_==(a)(d)).not
  /**
   * Matches if (a != b)
   */
  def be_!=(a: =>Any)(implicit d: Detailed) = (is_==(a)(d)).not
  /**
   * Matches if b is null
   */
  def beNull[T] = new BeNull[T]
  /**
   * Alias for beNull
   */
  def isNull[T] = beNull[T]
  /**
   * @deprecated use beAsNullAs
   */
  def beAlsoNull[T](a: =>T) = beAsNullAs(a)
  /**
   * Matches if a is null when v is null and a is not null when v is not null
   */
  def beAsNullAs[T](a: =>T) = new Matcher[T](){
    def apply(v: =>T) = {
      val x = a;
      val y = v;
      (x == null && y == null || x != null && y != null, "both values are null",
       if (x == null) d(y) + " is not null" else q(x) + " is not null" + description.map(" but " + _ + " is null").getOrElse(""))
    }
  }
  /**
   * Alias for beAsNullAs
   */
  def isAsNullAs[T](a: =>T) = beAsNullAs(a)
  /**
   * Matches if b is not null
   */
  def notBeNull[T] = beNull[T].not
  /**
   * Alias for notBeNull
   */
  def isNotNull[T] = notBeNull[T]
  /**
   * Matches if b is true
   */
  def beTrue = new Matcher[Boolean](){
    def apply(v: =>Boolean) = { val b = v; (b == true, description.getOrElse("the value") + " is true", description.getOrElse("the value") + " is false") }
  }
  /**
   * Alias for beTrue
   */
  def isTrue = beTrue
  /**
   * Matches if b is false
   */
  def beFalse[Boolean] = new Matcher[Boolean](){
    def apply(v: =>Boolean) = { val b = v; (b == false, description.getOrElse("the value") + " is false", description.getOrElse("the value") + " is true") }
  }
  /**
   * Alias for beFalse
   */
  def isFalse = beFalse
  /**
   * Matches if iterable.exists(_ == a)
   */
  def beIn[T](iterable: =>Iterable[T]): Matcher[T] = new Matcher[T](){
    def apply(v: => T) = {
      val (x, y) = (iterable, v)
      (x.exists(_ == y), d(y) + " is in " + q(x), d(y) + " is not in " + q(x))
    }
  }
  /**
   * Alias for beIn
   */
  def isIn[T](iterable: =>Iterable[T]): Matcher[T] = beIn(iterable)
  /**
   * Matches if not(iterable.exists(_ == a))
   */
  def notBeIn[T](iterable: =>Iterable[T]): Matcher[T] = beIn[T](iterable).not
  /**
   * Alias for notBeIn
   */
  def isNotIn[T](iterable: =>Iterable[T]): Matcher[T] = notBeIn(iterable)
  /**
   * Alias for notBeIn
   */
  def notIn[T](iterable: =>Iterable[T]): Matcher[T] = notBeIn(iterable)
  /**
   * Matches if t.toSeq.exists(_ == v)
   */
  def beOneOf[T](t: T*): Matcher[T] = new Matcher[T](){
    def apply(v: => T) = {
      val (x, y) = (t.toSeq, v)
      (x.exists(_ == y), d(y) + " is one of " + q(x.mkString(", ")), d(y) + " is not one of " + q(x.mkString(", ")))
    }
  }
  /**
   * Matches beOneOf
   */
  def isOneOf[T](t: T*): Matcher[T] = beOneOf(t:_*)
  /**
   * Matches if not(t.toSeq.exists(_ == v))
   */
  def notBeOneOf[T](t: T*): Matcher[T] = beOneOf(t:_*).not
  /**
   * Matches notBeOneOf
   */
  def isNotOneOf[T](t: T*): Matcher[T] = notBeOneOf(t:_*)
  /**
   * Matches notBeOneOf
   */
  def notOneOf[T](t: T*): Matcher[T] = notBeOneOf(t:_*)
  /**
   * Matches if the function f returns true
   */
  def verify[T](f: T => Boolean): Matcher[T] = new Matcher[T](){
     def apply(v: => T) = {val x = v; (f(x), description.getOrElse(x) + " verifies the property",
                                             description.getOrElse(x) + " doesn't verify the expected property")}
  }
  /**
   * Matches if value if an exception is thrown with the expected type
   * <br>Otherwise rethrow any other exception
   * <br>Usage: <code>value must throwA[SpecialException]</code>
   */
  def throwAnException[E <: Throwable](implicit m: ClassManifest[E]): ExceptionClassMatcher[E] = new ExceptionClassMatcher[E](m.erasure.asInstanceOf[Class[E]])

  /**
   * return a matcher which will be ok if an exception of that type is thrown
   */
  def throwA[E <: Throwable](implicit m: ClassManifest[E]): ExceptionClassMatcher[E] = throwAnException[E]
  /**
   * @see throwException description
   */
  def throwA[E <: Throwable](e: E): ExceptionMatcher[E] = throwException(e)
  /**
   * return a matcher which will be ok if an exception of that type is thrown
   */
  def throwAn[E <: Throwable](implicit m: ClassManifest[E]): ExceptionClassMatcher[E] = throwAnException[E]
  /**
   * Alias for throwA(new Exception)
   * @see throwException description
   */
  def throwAn[E <: Throwable](e: E): ExceptionMatcher[E] = throwException(e)
  /**
   * Alias for throwException
   * @deprecated use throwA or throwAn preferably
   */
  def throwThis[E <: Throwable](exception: =>E): ExceptionMatcher[E] = throwException(exception)
  /**
   * Matches if an exception is thrown with the expected class and message.
   * <br>Otherwise rethrow any other exception
   * <br>Usage: <code>value must throwA(new SpecialException)</code>
   * <br>Advanced usage: <code>value must throwA(new SpecialException).like {case ExceptionType(m) => m.startsWith("bad")}</code>
   * @deprecated use throwA or throwAn preferably
   */
  def throwException[E <: Throwable](exception: =>E): ExceptionMatcher[E] = new ExceptionMatcher[E](exception)
  /**
   * create an appropriate failure message for a thrown exception or exception class.
   * @see throwException
   */
  /**
   * Exception matcher checking the type of a thrown exception.
   */
  class ExceptionClassMatcher[E <: Throwable](exception: Class[E]) extends Matcher[Any] {
     def apply(value: => Any) = {
       (isThrown(value, exception, (e => exception.isAssignableFrom(e.getClass)), description).isDefined,
        okMessage(exception, description), koMessage(exception, description))
     }
    def like(f: =>PartialFunction[E, Boolean]) = new Matcher[Any](){
      def apply(v: => Any) = {
        val thrown = isThrown(v, exception, (e => exception.isAssignableFrom(e.getClass)), description)
        if (!thrown.isDefined)
          (false, okMessage(exception, description), koMessage(exception, description))
        else
          beLike(f).apply(thrown.get.asInstanceOf[E])
      }
    }
  }
  /**
   * This matchers matches exception objects.
   * @see throwException
   */
  class ExceptionMatcher[E <: Throwable](exception: E) extends Matcher[Any] {
     def apply(value: => Any) = {
       (isThrown(value, exception, (e => exception.getClass == e.getClass && exception.getMessage == e.getMessage), description).isDefined,
        okMessage(exception, description), koMessage(exception, description))
     }
     def like(f: =>PartialFunction[E, Boolean]) = new Matcher[Any](){
       def apply(v: => Any) = {
         val thrown = isThrown(v, exception, (e => exception.getClass.isAssignableFrom(e.getClass)), description)
         if (!thrown.isDefined)
           (false, okMessage(exception, description), koMessage(exception, description))
         else
           beLike(f).apply(thrown.get.asInstanceOf[E])
       }
     }
  }
  protected[matcher] def message(exception: Any) = {
    exception match {
      case e: Class[_] => e.toString.replaceFirst("class ", "")
      case ex: Throwable => ex.getClass.getName + ": " + ex.getMessage
      case other => other.toString
    }
  }
  protected[matcher] def okMessage(exception: Any, desc: Option[String]) = {
    message(exception) + " was thrown" + desc.map(" from " + _.toString).getOrElse("")
  }
  protected[matcher] def koMessage(exception: Any, desc: Option[String]) = message(exception) + " should have been thrown" + desc.map(" from " + _.toString).getOrElse("")

  /**
   * Creates a FailureException corresponding to a thrown exception.
   * <br>Sets the stacktrace of the <code>FailureException</code> so that it starts with the code line where the original exception
   * was thrown
   * @param e original exception
   * @param failureMessage exception message
   */
  private def throwFailure(e: =>Throwable, failureMessage: =>String) = {
    val failure = FailureException(failureMessage)
    failure.setStackTrace((e.getStackTrace.toList.dropWhile {x: StackTraceElement => x.toString.matches("AnyMatchers") || x.toString.matches("Expect")}).toArray)
    throw failure
  }

  /**
   * @returns an Option with the expected exception if it satisfies function <code>f</code>
   * <br>rethrows the exception otherwise
   */
  private def isThrown[E](value: => Any, expected: E, f: (Throwable => Boolean), desc: Option[String]) = {
    getException(value) match {
      case None => None
      case Some(e)  => if (f(e))
                         Some(e)
                       else
                         throwFailure(e, koMessage(expected, desc) + ". Got: " + e)
    }
  }
  /** evaluates a value and return any exception that is thrown */
  private def getException[E <: Throwable](value: => Any): Option[Throwable] = {
    try { value }
    catch { case e => { return Some(e)} }
    return None
  }

  /**
   * Matches if v.getClass == c
   */
  def haveClass[T](implicit m: ClassManifest[T]) = new Matcher[Any](){
    def apply(v: =>Any) = {
      val x: Any = v
      val c = m.erasure
      val xClass = x.asInstanceOf[java.lang.Object].getClass
      (xClass == c, d(x) + " has class" + q(c.getName), d(x) + " doesn't have class " + q(c.getName) + " but " + q(xClass.getName))
    }
  }

  /**
   * Matches if v.getClass != c
   */
  def notHaveClass[T](implicit m: ClassManifest[T]) = haveClass(m).not

  /**
   * Matches if v.isAssignableFrom(c)
   */
  def beAssignableFrom[T](implicit m: ClassManifest[T]) = new Matcher[Class[_]](){
    def apply(v: =>Class[_]) = {
      val x: Class[_] = v
      val c = m.erasure
      (x.isAssignableFrom(c), d(x.getName) + " is assignable from " + q(c.getName), d(x.getName) + " is not assignable from " + q(c.getName))
    }
  }

  /**
   * Matches if v.isAssignableFrom(c)
   */
  def notBeAssignableFrom[T](implicit m: ClassManifest[T]) = beAssignableFrom(m).not

  /**
   * Matches if c.isAssignableFrom(v)
   */
  def haveSuperClass[T](implicit m: ClassManifest[T]) = new Matcher[Any](){
    def apply(v: =>Any) = {
      val x: Any = v
      val c = m.erasure
      val xClass = x.asInstanceOf[java.lang.Object].getClass
      (c.isAssignableFrom(xClass), d(x) + " has super class" + q(c.getName), d(x) + " doesn't have super class " + q(c.getName))
    }
  }

  /**
   * Matches if c.isAssignableFrom(v)
   */
  def notHaveSuperClass[T](implicit m: ClassManifest[T]) = haveSuperClass(m).not

  /**
   * Adds functionalities to functions returning matchers so that they can be combined before taking a value and
   * returning actual matchers
   */
  implicit def toMatcher[S, T](f: S => Matcher[T]) = new ToMatcher(f)
  implicit def toMatcher2[T](f: T => Matcher[T]) = new ToMatcher2(f)

  /**
   * The <code>ToMatcher</code> class allows to combine functions returning matchers, or a function returning a matcher and a matcher.<p>
   * For example:<code>((beEqualTo(_:Int)) or (be_>(_:Int)))(3) </code>
   */
  class ToMatcher[S, T](f: S => Matcher[T]) {
    /**
     * @return a function which will return the and of 2 matchers
     */
    def and(m: =>Matcher[T]) = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s) and m
    }
    /**
     * @return a function which will return the or of 2 matchers
     */
    def or(m: =>Matcher[T]) = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s) or m
    }
    /**
     * @return a function which will return the xor of 2 matchers
     */
    def xor(m: Matcher[T]) = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s) xor m
    }
    /**
     * @return a function which will return the negation of a matcher
     */
    def not = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s).not
    }
    /**
     * @return a function which will return the and of 2 matchers
     */
    def and(m: S => Matcher[T]) = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s) and m(s)
    }
    /**
     * @return a function which will return the or of 2 matchers
     */
    def or(m: S => Matcher[T]) = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s) or m(s)
    }
    /**
     * @return a function which will return the xor of 2 matchers
     */
    def xor(m: S => Matcher[T]) = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s) xor m(s)
    }

    /**
     * @return a function which will return the composition of a matcher and a function
     */
    def ^^[A](g: A => S) = new Function1[A, Matcher[T]] {
      def apply(a: A) = f(g(a))
    }

    /**
     * @return a function which will return a matcher checking a sequence of objects
     */
    def toSeq = new Function1[Seq[S], Matcher[Seq[T]]] {
      def apply(s: Seq[S]) = new SeqMatcher(s, f)
    }

    /**
     * @return a function which will return a matcher checking a set of objects
     */
    def toSet = new Function1[Set[S], Matcher[Set[T]]] {
      def apply(s: Set[S]) = new SetMatcher(s, f)
    }
  }
  class ToMatcher2[T](f: T => Matcher[T]) {
    /**
     * @return a function which will return the composition of a matcher and a function
     */
    def ^^^[A](g: A => T) = {
      (a: A) => new Matcher[A] {
        def apply(b: =>A) = {
          f(g(a)).apply(g(b))
        }
      }
    }
  }

  /**
   * The <code>SeqMatcher</code> class is a matcher matching a sequence of objects with a matcher returned by a function.<p>
   * Usage:<code>List(1, 2, 3) must ((beEqualTo(_:Int)).toSeq)(List(1, 2, 3)) </code>
   */
  class SeqMatcher[S, T](s: Seq[S], f: S => Matcher[T]) extends Matcher[Seq[T]] {
    def apply(t: => Seq[T]) = {
      val (l1, l2) = (t, s)
      if (l1.size != l2.size) {
        (false, l1+" has the same size as "+l2, l1+" doesn't have the same size as "+l2)
      } else {
        val bothSequences = l1.toList zip l2.toList
        val results = bothSequences map { case (t1, s1) => f(s1).apply(t1) }
        (results.isEmpty || results.map(_._1).reduceLeft(_ && _), 
         results.filter(_._1).map(_._2).mkString("; "),
         results.filter(!_._1).map(_._3).mkString("; "))
      }
    }
  }

  /**
   * The <code>SetMatcher</code> class is a matcher matching a set of objects with a matcher returned by a function.<p>
   * Usage:<code>List(1, 2, 3) must ((beEqualTo(_:Int)).toSet)(List(2, 1, 3)) </code>
   */
  class SetMatcher[S, T](s: Set[S], f: S => Matcher[T]) extends Matcher[Set[T]] {
    def apply(t: => Set[T]) = {
      val setToTest = t
      if (s.size != setToTest.size)
        (false, "the sets contain the same number of elements",
                 d(setToTest) + " contains " + setToTest.size + " elements while " + q(s) + " contains " + s.size + " elements")
      else {
        val results = setToTest.map {(element: T) =>
          s.find { (otherElement:S) => f(otherElement).apply(element).success } match {
            case None => (false, "all matches", "no match for element " + q(element))
            case Some(x) => (true, q(element) + " matches with " + x, "no match for element " + q(element))
          }
        }
        (results.isEmpty || results.map(_._1).reduceLeft(_ && _), 
         results.filter(_._1).map(_._2).mkString("; "),
         results.filter(!_._1).map(_._3).mkString("; "))
      }
    }
  }
  /**
   * Alias of is_==
   */
  def ==(a: =>Any)(implicit d: Detailed) = is_==(a)(d)
  /**
   * Matches if (a != b)
   */
  def !=(a: =>Any)(implicit d: Detailed) = is_!=(a)(d)
}
trait StructuralMatchers {
  /** anything that can be empty */
  type HasIsEmptyMethod = Any { def isEmpty: Boolean }
  /** anything that can have a size */
  type HasSizeMethod = Any { def size: Int }
  class SizeMatcher(n: Int) extends Matcher[HasSizeMethod] {
	def apply(v: => HasSizeMethod) = {
	  val collection = v
	  (collection.size == n, d(collection) + " has size " + n, d(collection) + " doesn't have size " + n + ". It has size " + collection.size)
	}
  }
  /**
   * Matches if any object with an <code>isEmpty</code> method returns true: (Any {def isEmpty: Boolean}).isEmpty
   */
  def beEmpty = new Matcher[HasIsEmptyMethod](){
    def apply(v: => HasIsEmptyMethod) = {
      val iterable = v
      (iterable.isEmpty, dUnquoted(iterable) + " is empty", dUnquoted(iterable) + " is not empty")
    }
  }
  /**
   * Matches if not(beEmpty(a))
   */
  def notBeEmpty = beEmpty.not
  /**
   * Alias of notBeEmpty
   */
  def isNotEmpty = notBeEmpty
  /**
   * Alias of notBeEmpty
   */
  def notEmpty = notBeEmpty
  /**
   * Alias of beEmpty
   */
  def isEmpty = beEmpty
  /**
   * Matches if the size is n
   */
  def haveSize(n: Int) = new SizeMatcher(n)
}
trait AnyBeHaveMatchers { this: AnyBaseMatchers =>
  /** dummy matcher to allow be + matcher syntax */
  def be[T] = new BeVerbMatcher[T]
  /** dummy matcher to allow not + be + matcher syntax */
  def not[T] = new NotMatcher[T]
  /** dummy matcher to allow have + matcher syntax */
  def have[T] = new HaveVerbMatcher[T]
  /** dummy matcher to allow have + the matcher syntax */
  def the[T] = new ArticleMatcher[T]

  /** implicit definition to add 'be' matchers */
  implicit def toAnyResultMatcher[T](result: Result[T]) = new AnyResultMatcher(result)
  /** functions which can be used with 'be' matchers */
  class AnyResultMatcher[T](result: Result[T]) {
    def equalTo(o: T)(implicit d: Detailed) = result.matchWith(is_==(o)(d))
    def ==(a: =>Any)(implicit d: Detailed) = result.matchWith(is_==(a)(d))
    def !=(a: =>Any)(implicit d: Detailed) = result.matchWith(is_!=(a)(d))
    def asNullAs(a: =>T) = result.matchWith(beAsNullAs(a))
    def in(iterable: =>Iterable[T]) = result.matchWith(beIn(iterable))
    def oneOf(t: T*) = result.matchWith(beOneOf(t:_*))
    def throwA[E <: Throwable](implicit m: ClassManifest[E]) = result.matchWith(throwAnException[E](m))
    def throwA[E <: Throwable](e: E) = result.matchWith(throwException(e))
    def throwAn[E <: Throwable](implicit m: ClassManifest[E]) = result.matchWith(throwAnException[E](m))
    def throwAn[E <: Throwable](e: E) = result.matchWith(throwException(e))
  }
  /** implicit definition to add 'empty' matchers */
  implicit def toAnyEmptyResultMatcher[S](result: Result[S])(implicit c: S => HasIsEmptyMethod): AnyEmptyResultMatcher[S] = new AnyEmptyResultMatcher(result)(c)
  /** functions which can be used with 'empty' matchers */
  class AnyEmptyResultMatcher[S](result: Result[S])(implicit c: S => HasIsEmptyMethod) {
    def empty = result.matchWith(beEmpty ^^ c)
  }
  /** implicit definition to add 'size' matchers */
  implicit def toAnySizeResultMatcher[S](result: Result[S])(implicit c: S => HasSizeMethod): AnySizeResultMatcher[S] = new AnySizeResultMatcher(result)(c)
  /** functions which can be used with 'size' matchers */
  class AnySizeResultMatcher[S](result: Result[S])(implicit c: S => HasSizeMethod) {
    def size(n: Int) = result.matchWith(haveSize(n) ^^ c)
  }
  /**
   * Alias of is_==
   */
  def equalTo[T](o: T)(implicit d: Detailed) = is_==(o)(d)
  /**
   * Alias for beAsNullAs
   */
  def asNullAs[T](a: =>T) = beAsNullAs(a)
  /**
   * Alias for beIn
   */
  def in[T](iterable: =>Iterable[T]): Matcher[T] = beIn(iterable)
  /**
   * Matches beOneOf
   */
  def oneOf[T](t: T*): Matcher[T] = beOneOf(t:_*)
  /**
   * Alias of beEmpty
   */
  def empty = beEmpty
}
/**
 * Reusable equalTo matcher
 */
class BeEqualTo[T](a: =>T) extends Matcher[T] {
  def apply(v: =>T) = {
    val (x, y) = (a, v)
    var dy = d(y)
    var qx = q(x)
   	var differentTypesWarning = ""
    if (dy == qx) {
	  val xClass = getClassName(x)
	  val yClass = getClassName(y)
	  if (xClass != yClass) {
        dy = dy + ": " + yClass
        qx = qx + ": " + xClass
	  } else differentTypesWarning = ". Values have the same string representation but possibly different types like List[Int] and List[String]"
	}
    (x == y, dy + " is equal to " + qx, dy + " is not equal to " + qx + differentTypesWarning)
  }
}
class BeNull[T] extends Matcher[T] {
  def apply(v: =>T) = { val b = v; (b == null, description.getOrElse("the value") + " is null", d(b) + " is not null") }
}
