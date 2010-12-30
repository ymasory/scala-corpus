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
package org.specs.matcher;
import MatcherUtils._
import java.util.regex._
import org.specs.specification._
/**
 * The <code>StringMatchers</code> trait provides matchers which are applicable to String objects
 */
trait StringMatchers extends StringBaseMatchers with StringBeHaveMatchers 
trait StringBaseMatchers { outer =>
  
  /**
   * Matches if (a.equalsIgnoreCase(b))
   */   
  def beEqualToIgnoringCase[T <: String](a: T) = new BeEqualToIgnoringCase(a)
  /**
   * Matches if (a.equalsIgnoreCase(b))
   */   
  def be_==/[T <: String](a: T) = beEqualToIgnoringCase(a)  
  /**
   * Matches if (a.equalsIgnoreCase(b))
   * @deprecated use equalToIgnoringCase instead
   */   
  def equalIgnoreCase[T <: String](a: T) = beEqualToIgnoringCase(a)

  /**
   * Matches if (a.notEqualIgnoreCase(b))
   */   
  def be_!=/[T <: String](a: T) = notEqualIgnoreCase(a)  

  /**
   * Matches if !(a.equalsIgnoreCase(b))
   * @deprecated use notBeEqualToIgnoringCase instead
   */   
  def notEqualIgnoreCase[T <: String](a: T) = beEqualToIgnoringCase(a).not 
  /**
   * Matches if !(a.equalsIgnoreCase(b))
   */   
  def notBeEqualToIgnoringCase[T <: String](a: T) = beEqualToIgnoringCase(a).not 

  /**
   * Matches if (a.trim == b.trim)
   */   
  def beEqualToIgnoringSpace[T <: String](a: T) = new Matcher[T](){ 
    def apply(v: => T) = {val b = v; (a != null && b != null && a.trim == b.trim, 
                                      d(b) + " is equal ignoring space to " + q(a), 
                                      d(b) + " is not equal ignoring space to " + q(a))} 
  }

  /**
   * Matches if (a.trim == b.trim)
   * @deprecated use beEqualToIgnoringSpace instead
   */   
  def equalIgnoreSpace[T <: String](a: T) = beEqualToIgnoringSpace(a)
  /**
   * Matches if !(a.equalsIgnoreSpace(b))
   * @deprecated use notBeEqualToIgnoringSpace instead
   */   
  def notEqualIgnoreSpace[T <: String](a: T) = beEqualToIgnoringSpace(a).not 
  /**
   * Matches if !(a.equalsIgnoreSpace(b))
   */   
  def notBeEqualToIgnoringSpace[T <: String](a: T) = beEqualToIgnoringSpace(a).not 

  /**
   * Matches if (b.indexOf(a) >= 0)
   */   
  def include[T <: String](a: String) = new Matcher[T](){ 
    def apply(v: => T) = {val b = v; (a != null && b != null && b.indexOf(a) >= 0, 
                                      d(b) + " includes " + q(a), 
                                      d(b) + " doesn't include " + q(a))} 
  }

  /**
   * Matches if !(b.indexOf(a) >= 0)
   */   
  def notInclude[T <: String](a: T) = include[T](a).not 

  /**
   * Matches if b matches the regular expression a
   */   
  def beMatching[T <: String](a: T) = new Matcher[T](){
    def apply(v: => T) = {val b = v; (matches(a)(b), 
                                      d(b) + " matches " + q(a), 
                                      d(b) + " doesn't match " + q(a))}
  }

  /**
   * Matches if b doesn't match the regular expression a
   */   
  def notBeMatching(a: String) = beMatching[String](a).not

  /**
   * Matches if b.startsWith(a)
   */   
  def startWith[T <: String](a: T) = new Matcher[T](){ 
     def apply(v: => T) = {val b = v; (b!= null && a!= null && b.startsWith(a), 
                                       d(b) + " starts with " + q(a), 
                                       d(b) + " doesn't start with " + q(a))} 
  }
  /**
   * Matches if !b.startsWith(a)
   */   
  def notStartWith(a: String) = startWith[String](a).not

  /**
   * Matches if b.endsWith(a)
   */   
  def endWith[T <: String](a: T) = new Matcher[T](){ 
     def apply(v: => T) = {val b = v; (a != null && b != null && b.endsWith(a), 
                                       d(b) + " ends with " + q(a), 
                                       d(b) + " doesn't end with " + q(a))} 
  }

  /**
   * Matches if !b.endsWith(a)
   */   
  def notEndWith(a: String) = endWith[String](a).not

  /**
   * Matches if the regexp a is found inside b
   */   
  def find[T <: String](a: T) = new FindMatcher(a)

  /**
   * Matcher to find if the regexp a is found inside b. 
   * This matcher can be specialized to a FindMatcherWithGroups which will also check the found groups
   */   
  class FindMatcher[T <: String](a: T) extends Matcher[T] {
    def found(b: T) = {
      val matcher = Pattern.compile(a).matcher(b)
      matcher.find
    }
    def withGroup(group: String) = new FindMatcherWithGroups(a, group)
    def withGroups(groups: String*) = new FindMatcherWithGroups(a, groups:_*)
    def apply(v: => T) = {val b = v; (a != null && b != null && found(b), 
                                      q(a) + " is found in " + d(b), 
                                      q(a) + " isn't found in " + d(b))} 
  }

  /**
   * Matcher to find if the regexp a is found inside b. 
   * This matcher checks if the found groups are really the ones expected
   */   
  class FindMatcherWithGroups[T <: String](a: T, groups: String*) extends Matcher[T] {
    def found(b: T) = {
      val matcher = Pattern.compile(a).matcher(b)
      val groupsFound = new scala.collection.mutable.ListBuffer[String]()
      while (matcher.find) { groupsFound += matcher.group(1) }
      groupsFound.toList
    }
    def apply(v: => T) = {
      val b = v
      val groupsFound = found(b)
      val withGroups = if (groups.size > 1) " with groups " else " with group "
      def foundText = {
        if (groupsFound.isEmpty) 
          ". Found nothing" 
        else 
           ". Found: " + q(groupsFound.mkString(", "))
      }
      val groupsToFind = if (groups == null) Nil else groups.toList
      (a != null && b != null && groupsFound == groupsToFind, 
       q(a) + " is found in " + d(b) + withGroups + q(groupsToFind.mkString(", ")), 
       q(a) + " isn't found in " + d(b) + withGroups + q(groupsToFind.mkString(", ")) + foundText 
       )
    } 
  }
  /**
   * Matches if the length is n
   */
  def haveLength(n: Int) = new Matcher[String](){
    def apply(v: => String) = {val string = v; (string.length == n, d(string) + " has length " + n, d(string) + " doesn't have length " + n)}
  }
  def ==/(s: String) = be_==/(s)
  def !=/(s: String) = be_!=/(s)
  def equalToIgnoringCase[T <: String](a: T) = beEqualToIgnoringCase(a)
  def equalToIgnoringSpace[T <: String](a: T) = beEqualToIgnoringSpace(a)
  def equalIgnoringCaseTo[T <: String](a: T) = beEqualToIgnoringCase(a)
  def equalIgnoringSpaceTo[T <: String](a: T) = beEqualToIgnoringSpace(a)
}
trait StringBeHaveMatchers { outer: StringBaseMatchers =>
  implicit def toStringResultMatcher(result: Result[String]) = new StringResultMatcher(result)
  class StringResultMatcher(result: Result[String]) {
    def matching(s: String) = result.matchWith(beMatching(s))
    def equalToIgnoringCase(s: String) = result.matchWith(be_==/(s))
    def equalIgnoringCaseTo(s: String) = result.matchWith(be_==/(s))
    def equalToIgnoringSpace(s: String) = result.matchWith(beEqualToIgnoringSpace(s))
    def equalIgnoringSpaceTo(s: String) = result.matchWith(beEqualToIgnoringSpace(s))
    def ==/(s: String) = result.matchWith(be_==/(s))
    def !=/(s: String) = result.matchWith(be_!=/(s))
    def empty = result.matchWith(new StringEmptyMatcher)
    def size(n: Int) = result.matchWith(haveLength(n))
    def length(n: Int) = result.matchWith(haveLength(n))
    def include(s: String) = result.matchWith(outer.include(s))
    def startWith(s: String) = result.matchWith(outer.startWith(s))
    def endWith(s: String) = result.matchWith(outer.endWith(s))
  }
  def length(n: Int) = haveLength(n)
  def matching(s: String) = beMatching(s)
}
class StringEmptyMatcher extends Matcher[String] {
  def apply(v: => String) = {
    val s = v
    (s.isEmpty, dUnquoted(s) + " is empty", dUnquoted(s) + " is not empty")
  }
}
class BeEqualToIgnoringCase[T <: String](a: T) extends Matcher[T] { 
  def apply(v: => T) = {val b = v; (a != null && b != null && a.equalsIgnoreCase(b), 
                                      d(b) + " is equal ignoring case to " + q(a), 
                                      d(b) + " is not equal ignoring case to " + q(a))} 
}