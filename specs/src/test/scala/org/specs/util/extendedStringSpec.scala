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
import org.specs._
import org.specs.runner._
import org.specs.util.ExtendedString._

class extendedStringSpec extends SpecificationWithJUnit with DataTables {
  "the uncapitalize function" should {
    "lower-case only the first letter of a string" in {
      "Hello".uncapitalize must_== "hello"
    }
    "lower-case only the first letter of a one letter string" in {
      "H".uncapitalize must_== "h"
    }
  }
  "the uncamel function" should {
    "uncapitalize words except the first one and insert spaces in a camel word" in {
      "source"      | "target"          |>
      ""            ! ""                |  
      "my"          ! "my"              |  
      "My"          ! "My"              |  
      "MyClass"     ! "My class"        |  
      "myClass"     ! "my class"        |  
      "MyClassName" ! "My class name"   | { (source, target) => 
        source.uncamel must_== target 
      }
    }
  }
  "the removeAll function" should {
    "remove a simple character" in {
      "hello".removeAll("l") must_== "heo"
    }
    "remove two characters" in {
      "hello".removeAll("lo") must_== "hel"
    }
    "remove regexp characters" in {
      "he(l)(l)o".removeAll(")(") must_== "he(ll)o"
    }
  }
  "the removeFrom function" should {
    "remove everything from a given substring" in {
      "hello$world$hi".removeFrom("$") must_== "hello"
    }
  }
  "the groups function" should {
    "return Nil if the pattern is null" in {
      "hello".groups(null) must beEmpty
    }
    "return Nil if no group is found in a string according to a pattern" in {
      "hello".groups("(z)") must beEmpty
    }
    "return Nil if no group is found in a string according to a pattern, even if parenthesis are omitted" in {
      "hello".groups("z") must beEmpty
    }
    "return the found group if there is only one" in {
      "hello".groups("(e)") must_== List("e")
    }
    "return the found groups when there are several" in {
      "hello".groups("(l)") must_== List("l", "l")
    }
    "return nothing if the parenthesis are omitted, even if there's a match" in {
      "hello".groups("l") must beEmpty
    }
  }
  "the replaceGroups function" should {
    "leave the string as it is if nothing is found" in {
      "hello".replaceGroups("(z)", (s: String) => s.size) must_== "hello"
    }
    "replace every found group with the application of a function" in {
      "hello".replaceGroups("(l)", (s: String) => s.size) must_== "he11o"
    }
    "replace every found group with the application of a function, replacing only the group" in {
      "wor<code>l</code>d".replaceGroups("<code>(l)</code>", (s: String) => s.size) must_== "wor<code>1</code>d"
    }
  }
  "the findAll function" should {
    "return Nil if the pattern is null" in {
      "hello".findAll(null) must beEmpty
    }
    "return Nil if no group is found in a string according to a pattern" in {
      "hello".findAll("z") must beEmpty
    }
    "return the found group if there is only one" in {
      "hello".findAll("e") must_== List("e")
    }
    "return the found groups when there are several" in {
      "hello".findAll("l") must_== List("l", "l")
    }
  }
  "the splitToSize function" should {
    "not split a string if its size is less than or equal the split size" in {
      "hello".splitToSize(5) must_== List("hello")
    }
    "split a string if its size is greater than the split size" in {
      "hello".splitToSize(3) must_== List("hel", "lo")
      "hello world".splitToSize(3) must_== List("hel", "lo ", "wor", "ld")
    }
  }
}
