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
import org.specs.mock._
import org.specs.Sugar._
import org.specs.specification._
import org.specs.execute._

class anyMatchersUnit extends MatchersSpecification {
  "A 'be' matcher" should {
    "be ok if comparing the same object, like the eq method" in {
      "string" must be("string")
    }
    "display a failure message if comparing different objects" in {
      expectation("name" must be("name2")) must failWith("'name' is not the same as 'name2'")
    }
    "be resilient to a null value" in {
      expectation("name" must be(null)) must failWith("'name' is not the same as 'null'")
    }
    "display a failure message if comparing different objects even if they are ==" in {
      case class MyObject(value: Int)
      val (o1, o2) = (MyObject(1), MyObject(1))
      expectation(o1 must be(o2)) must failWith("'MyObject(1)' is not the same as 'MyObject(1)'")
    }
    "be ok if comparing the same value" in {
      1 must be(1)
    }
    "display a failure message if comparing different values" in {
      expectation(1 must be(2)) must failWith("'1' is not the same as '2'")
    }
    "display a precise failure message if there is an alias" in {
      expectation(1 aka "the number" must be(2)) must failWith("the number '1' is not the same as '2'")
    }
    "be ok when matching a Class object" in {
      class Baz
	  def calcBar = classOf[Baz]
	  calcBar mustEqual classOf[Baz]
	}
  }
  "An '==' matcher" should {
    "be ok if comparing 2 objects which are equals with ==" in {
      case class MyObject(value: Int)
      val (o1, o2) = (MyObject(1), MyObject(1))
      o1 must be_==(o2)
    }
    "be ok when using the short form ==" in {
      1 must ==(1)
    }
    "be ok when comparing a list of ints" in {
      List(1) must be_==(List(1))
    }
    "be ok when comparing a list of strings" in {
      List("a") must be_==(List("a"))
    }
    "display a failure message if comparing different objects" in {
      expectation("name" must be_==("name2")) must failWith("'name' is not equal to 'name2'")
    }
    "be resilient to a null value" in {
      expectation("name" must be_==(null)) must failWith("'name' is not equal to 'null'")
    }
    "provide the type of the objects in the failure message when their toString method return the same value" in {
      val d: Double = 0.1
      val f: Float = 0.1f
      expectation(f must_== d) must failWith("'0.1': Float is not equal to '0.1': Double")
    }
    "provide the type of the objects in the failure message when their toString method return the same value, with containers" in {
      expectation(List(1) must_== List("1")) must failWith("'List(1)' is not equal to 'List(1)'. Values have the same string representation but possibly different types like List[Int] and List[String]")
    }
  }
  "An 'beEqualTo' matcher" should {
    "be ok if comparing 2 objects which are equals with ==" in {
      case class MyObject(value: Int)
      val (o1, o2) = (MyObject(1), MyObject(1))
      o1 must beEqualTo(o2)
    }
    "be ok when comparing a list of ints" in {
      List(1) must beEqualTo(List(1))
    }
    "be ok when comparing a list of strings" in {
      List("a") must beEqualTo(List("a"))
    }
    "display a failure message if comparing different objects" in {
      expectation("name" must beEqualTo("name2")) must failWith("'name' is not equal to 'name2'")
    }
    "be resilient to a null value" in {
      expectation("name" must beEqualTo(null:String)) must failWith("'name' is not equal to 'null'")
    }
  }
  "A 'beNull' matcher" should {
    "be ok if comparing with a null object" in {
      (null:String) must beNull
    }
    "display a failure message if the value is not null" in {
      expectation("not null" must beNull) must failWith("'not null' is not null")
    }
    "display a precise failure message if there is a description of the value" in {
      expectation("not null" aka "the value" must beNull) must failWith("the value 'not null' is not null")
    }
  }
  "A 'notBeNull' matcher" should {
    "be ok if comparing with a non-null object" in {
      "" must notBeNull
    }
    "display a failure message if the value is null" in {
      expectation((null:String) must notBeNull) must failWith("the value is null")
    }
    "display a precise failure message if there is a description of the value" in {
      expectation((null:String) aka "this value" must notBeNull) must failWith("this value is null")
    }
  }
  "A 'beTrue' matcher" should {
    "be ok if comparing with a true object" in {
      true must beTrue
    }
    "display a failure message if the value is not true" in {
      expectation(false must beTrue) must failWith("the value is false")
    }
    "display a precise failure message if the value has a description" in {
      expectation(false aka "this value" must beTrue) must failWith("this value is false")
    }
  }
  "A 'beFalse' matcher" should {
    "be ok if comparing with a false object" in {
      false must beFalse
    }
    "display a failure message if the value is not true" in {
      expectation(true must beFalse) must failWith("the value is true")
    }
    "display a precise failure message if the value has a description" in {
      expectation(true aka "this value" must beFalse) must failWith("this value is true")
    }
  }
  "A 'beEmpty' matcher" should {
    "be ok when testing an empty string" in {
      "" must be empty
    }
  }
  "A throwA + exception matcher" should {
    "be ok if a value throws the expected exception type" in {
      throwThis(new java.lang.Error("test"))(throw new java.lang.Error("test")) must beLike { case (true, _, _) => ok }
    }
    "be ko if the value doesn't throw any exception" in {
      throwThis(new Exception)(1) must beLike { case (false, _, message) => ok }
    }
    "specify the expected exception in the failure message" in {
      throwThis(new Exception)(1)._3 must include((new Exception).getClass.getName)
    }
    "throw a Failure exception if the value throws another exception" in {
      val matcher: ExceptionClassMatcher[java.lang.Error] = throwA[java.lang.Error]
      matcher(throw new Exception) must throwA[FailureException]
    }
    "throw a Failure exception with the other exception message, if the value throws another exception" in {
      throwThis(new java.lang.Error("Error"))(throw new Exception) must throwThis(new FailureException("java.lang.Error: Error should have been thrown. Got: java.lang.Exception"))
    }
    "display a precise failure message if the block has a description" in {
      lazy val block = { throw new Exception  }
      { theBlock(block) aka "this block" must throwThis(new java.lang.Error("Error")) } must throwThis(new FailureException("java.lang.Error: Error should have been thrown from this block. Got: java.lang.Exception"))
    }
  }
  "the message function" should {
    "return the message of the exception if the parameter is an exception" in {
      message(new java.lang.Error("buzz")) must_== "java.lang.Error: buzz"
    }
    "return the name of the exception class if the parameter is an exception class" in {
      message(classOf[Error]) must_== "java.lang.Error"
    }
  }
  case class SimpleException(s: String) extends Exception(s)
  "A throwThis + exception matcher" should {
    "be ok if a value throws an exception equals to the expected exception one" in {
      throwThis(SimpleException("Message"))(throw SimpleException("Message"))._1 mustBe true
    }
  }
  "A throwA + exception matcher" can {
    "specify a like clause to add pattern matching" in {
      throwThis(SimpleException("Message")).like { case SimpleException(x) => !x.isEmpty}(
          throw new SimpleException("Message")) must beLike {case (true, _, _) => ok}
    }
    "specify a like clause to add pattern matching for a simple exception, with type inference" in {
      class MyException extends Throwable { def foo = 42 }
      throwThis(new MyException).like { case e => e.foo == 42 }(
          throw new MyException)._1 must beTrue
    }
  }
  "Any matchers" should {
    val anyValue: Any = 1
    val nullValue: Int = null.asInstanceOf[Int]
    val boolValue: Boolean = true

    "not evaluate the expressions twice: be_!=" in {
      be_!=(1) must evalOnce(exp(anyValue))
    }
    "not evaluate the expressions twice: be_==" in {
      be_==(1) must evalOnce(exp(anyValue))
    }
    "not evaluate the expressions twice: be_==" in {
      val expression = exp(anyValue)
      be_==(expression.evaluate).apply(1)
      expression.evaluationsNb must_== 1
    }
    "not evaluate the expressions twice: be" in {
      be(1) must evalOnce(exp(anyValue))
    }
    "not evaluate the expressions twice: beNull" in {
      beNull[Int] must evalOnce(exp(nullValue))
    }
    "not evaluate the expressions twice: verify" in {
      verify((x:Int) => x == 1) must evalOnce(exp(1))
    }
    "not evaluate the expressions twice: beTrue" in {
      beTrue must evalOnce(exp(boolValue))
    }
    "not evaluate the expressions twice: beFalse" in {
      (beFalse: Matcher[Boolean]) must evalOnce(exp(boolValue))
    }
  }
  "a matcher" should {
    "use the description if passed one" in {
      val m: Matcher[Boolean] = beTrue
      m.setDescription(Some("this expression"))
      m.apply(true) must_== (true, "this expression is true", "this expression is false")
    }
  }
}
