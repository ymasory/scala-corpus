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
import org.specs.Sugar._

class objectMatchersSpec extends MatchersSpecification {
  "Object matchers" should { 
    "provide a 'must_==' matcher: 'name' must_== 'name'" in {
      "string" must_== "string"
      expectation("string" must_== "string2") must failWith("'string' is not equal to 'string2'")
    }
    "provide a 'must_!=' matcher 'name' must_!= 'name2'" in {
      "string" must_!= "string2"
      expectation("string" must_!= "string") must failWith("'string' is equal to 'string'")
    }
    "provide a 'must be' matcher: o1 must be(o2) if they are the same object " +
      "('must eq' cannot be used because it overrides the eq matcher from Object) [alias: mustBe, mustEq]" in {
        val o1 = new Object {override def toString = {"o1"}}
        val o3 = new Object {override def toString = {"o3"}}
        val o2 = o1
        o1 must be(o2)
        o1 mustBe o2
        o1 mustEq o2
        expectation(o1 must be(o3)) must failWith("'o1' is not the same as 'o3'")
    }
    "provide a 'must notBe' matcher: o1 must notBe(o2) if they are not the same object [alias: mustNotBe, mustNotEq]" in {
        val o1 = new Object {override def toString = {"o1"}}
        val o3 = new Object {override def toString = {"o3"}}
        val o2 = o1

        o1 must notBe(o3)
        o1 mustNotBe o3
        o1 mustNotEq o3
        expectation(o1 must notBe(o2)) must failWith("'o1' is the same as 'o1'")
    }
    "provide a 'verifies' matcher checking if an object verifies a property: 'name' verifies {_.size == 4} [alias: verify]" in {
      List() verifies { _.isEmpty }
      expectation(List("1") verifies { _.isEmpty }) must failWith("List(1) doesn't verify the expected property")
    }
    "provide a 'mustThrow' matcher expecting a block to send an exception of a given type" in {
      {throw new Error("user error");()} must throwAn[Error]

      class MyError(msg: String) extends Error(msg) {}
      {throw new MyError("subclass of error");()} must throwAn[Error]

      expectation({throw new NullPointerException;()} must throwAn[Error]) must failWith("java.lang.Error should have been thrown. Got: java.lang.NullPointerException")
    }
    "provide a throwAn[T] matcher expecting an exception" in {
      {throw new Error("user error");()} must throwAn[Error]

      {throw new RuntimeException("e");()} must throwA[RuntimeException]
    }
    "provide a beAsNullAs matcher which will check if 2 objects are null at the same time" in {
      val nullString: String = null
      nullString must beAsNullAs(nullString)
      1 must beAsNullAs(1)
      expectation(nullString must beAsNullAs("not null")) must failWith("'not null' is not null")
      expectation(nullString aka "the string" must beAsNullAs("not null")) must failWith("'not null' is not null but the string is null")

      expectation("not null" must beAsNullAs(nullString)) must failWith("'not null' is not null")
      expectation("not null" aka "the string" must beAsNullAs(nullString)) must failWith("the string 'not null' is not null")
    }
    "provide a haveClass matcher checking if any.getClass == c" in {
      val a: Any = 1
      a must haveClass[java.lang.Integer]
      expectation(a must haveClass[String]) must failWith("'1' doesn't have class 'java.lang.String' but 'java.lang.Integer'")
      expectation(a aka "the object" must haveClass[String]) must failWith("the object '1' doesn't have class 'java.lang.String' but 'java.lang.Integer'")
    }
    "provide a haveClass matcher checking if any.getClass == c - with String" in {
      val a: Any = "string"
      a must haveClass[String]
      expectation(a must haveClass[java.lang.Integer]) must failWith("'string' doesn't have class 'java.lang.Integer' but 'java.lang.String'")
    }
    "provide a haveSuperClass matcher checking if c isAssignableFrom any.getClass" in {
      val a: Any = new java.io.FileOutputStream(new java.io.FileDescriptor) { override def toString = "FileOutputStream"}
      a must haveSuperClass[java.io.OutputStream]
      expectation(a must haveSuperClass[java.lang.String]) must failWith("'FileOutputStream' doesn't have super class 'java.lang.String'")
      expectation(a aka "the object" must haveSuperClass[java.lang.String]) must failWith("the object 'FileOutputStream' doesn't have super class 'java.lang.String'")
    }
    "provide a beAssignableFrom matcher checking if any.getClass isAssignableFrom c" in {
      val a: Object = new java.io.FileOutputStream(new java.io.FileDescriptor)
      classOf[java.io.OutputStream] must beAssignableFrom[java.io.FileOutputStream]
      expectation(classOf[java.io.OutputStream] must beAssignableFrom[String]) must failWith("'java.io.OutputStream' is not assignable from 'java.lang.String'")
      expectation(classOf[java.io.OutputStream] aka "the class" must beAssignableFrom[String]) must failWith("the class 'java.io.OutputStream' is not assignable from 'java.lang.String'")
    }
  }
}
