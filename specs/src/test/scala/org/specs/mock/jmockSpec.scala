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
package org.specs.mock
import org.specs.runner._
import org.specs.io.mock._
import org.specs.mock._
import org.specs.Sugar._
import org.specs.specification._
import org.specs.runner._
import org.hamcrest.core._
import org.specs.matcher._
import org.specs._

class jmockSpec extends SpecificationWithJUnit {
  "The jMock integration".isSpecifiedBy(jmockGoodSpecification, jmockBadSpecification, countingNamingSchemeSpecification)
}
object jmockGoodSpecification extends Mocked {
  "The JMocker trait" should {
    "provide a 'one' method succeeding if only one method is called" in {
      expect { 
        one(list).size 
      }
      list.size
    }
    "provide an 'exactly' method succeeding if exactly the right number of calls are made" in {
      expect { 2.of(list).size }
      2 times {i => list.size}
    }
    "provide an 'atLeast' method succeeding if atLeast the right number of calls are made" in {
      expect { 2.atLeastOf(list).size }
      3 times {i => list.size}
    }
    "provide a 'between' method succeeding if the number of calls is between inside a range" in {
      expect { (2 to 4).of(list).size }
      3 times {i => list.size}
    }
    "provide an 'atMost' method succeeding if at most the right number of calls are made" in {
      expect { 2.atMostOf(list).size }
      2 times {i => list.size}
    }
    "provide an 'allowing' method succeeding if any number of calls is made" in {
      expect { allowing(list).size }
      2 times {i => list.size}
    }
    "provide an 'allowingMatch' method succeeding if calls match a method pattern" in {
      expect { allowingMatch("size") }
      list.size
    }
    "provide an 'allowingMatch' method succeeding if calls match a method pattern on a specific mock" in {
      expect { allowingMatch(list, "size") }
      list.size
    }
    "provide an 'ignoring' method accepting any call and returning default values" in {
      expect { ignoring(list) }
      list.size must_== 0
    }
    "provide an 'ignoring' method where toString on a mock returns the mock name build following the class name" in {
      expect { ignoring(list) }
      list.toString must_== "list"
    }
    "provide an 'ignoringMatch' method accepting any call matching a method pattern and returning default values" in {
      expect { ignoringMatch("size") }
      list.size must_== 0
    }
    "provide a 'never' method succeeding if no call is made to the mock" in {
      expect { never(list) }
    }
    "provide an anyInt matcher which can be used to specify that any Int will be used as a parameter" in {
      expect { 1.of(list).get(anyInt) }
      list.get(0)
    }
    class Param(name: String)
    class ToMockWithParams { def method(p: Param*) = () }
    "provide an any[Type] matcher which can match any parameter of a given type with vargs" in {
      val mocked: ToMockWithParams = mock[ToMockWithParams]
      expect { 1.of(mocked).method(any[Param]) }
      mocked.method(new Param("hello"))
    }
    "provide an a[T] matcher which can be used to specify that any instance of type T will match - alias an[T]" in {
      val listString: List[String] = mock[List[String]]
      expect { 0.atLeastOf(listString).mkString(a[String]) }
      listString.mkString(",")
      listString.mkString("+")
    }
    "provide an aNull[X] matcher which can be used to specify that a null value of class X will be used as a parameter" in {
      val listString: List[String] = mockAs[List[String]]("list of strings")
      expect { 1.of(listString).mkString(aNull[String]) }
      listString.mkString(null)
    }
    "provide an equal matcher which can be used to specify that a specific value will be used as a parameter" in {
      expect { 1.of(list).get(equal(0)) }
      list.get(0)
    }
    class ParamMock { def method(p1: Int, p2: Int) = () }
    "provide an equal matcher which can be used to specify that a specific value will be used as a parameter - with 2 paramters" in {
      val mocked: ParamMock = mock[ParamMock]
      expect { 1.of(mocked).method(equal(0), equal(1)) }
      mocked.method(0, 1)
    }
    "provide a will method, using a Hamcrest matcher, to specify that a specific value will be used as a parameter" in {
      expect { 1.of(list).get(will(new IsEqual(0))) }
      list.get(0)
    }
    "provide a will method, using a specs matcher, to specify that a specific value will be used as a parameter" in {
      expect { 1.of(list).get(will(beEqualTo(0))) }
      list.get(0)
    }
    "provide a willReturn method to specify the value which must be returned" in {
      expect { 1.of(list).get(will(beEqualTo(0))) willReturn "new" }
      list.get(0) must_== "new"
    }
    "provide a willThrow method to specify the exception which must be thrown" in {
      expect { 1.of(list).get(will(beEqualTo(0))) willThrow new java.lang.Exception("ouch") }
      list.get(0) must throwAn[Exception]
    }
    "provide a willReturn method to specify the a returned iterator" in {
      val expected = List[String]("hey")
      expect { 1.of(scalaList).iterator willReturn expected.iterator }
      scalaList.iterator.next must_== "hey"
    }
    "provide a willReturn method to specify a returned iterable" in {
      expect { 1.of(scalaList).take(anyInt) willReturn List("hey") }
      scalaList.take(1) must containMatch("hey")
    }
    "provide a willReturn method accepting a block to return another mock and specify it too" in {
      case class Module(name: String)
      case class Project(module: Module, name: String)
      case class Workspace(project: Project)
      val workspace: Workspace = mock[Workspace]

      expect {
        1.atLeastOf(workspace).project.willReturn[Project] {p: Project =>
           1.atLeastOf(p).name willReturn "hi"
           1.atLeastOf(p).module.willReturn[Module]{ (m: Module) => 1.of(m).name willReturn "module"}
        }
      }
      workspace.project.name must_== "hi"
      workspace.project.module.name must_== "module"
    }
    "provide a willReturn method returning iterables and accepting a block to return another mock and specify it too" in {
      case class Project(name: String)
      case class Workspace(projects: List[Project])
      val workspace: Workspace = mock[Workspace]
      expect {
        one(workspace).projects willReturnIterable(
          as[Project]("p1"){p: Project =>
            one(p).name willReturn "p1" },
          as[Project]("p2"){p: Project =>
            one(p).name willReturn "p2" }
        )
      }
      workspace.projects.map(_.name) must_== List("p1", "p2")
    }
    "provide a willReturn method returning iterables and accepting a block to return another mock and specify it too - 2" in {
      case class Project(name: String)
      case class Workspace(projects: List[Project])
      val workspace: Workspace = mock[Workspace]
      expect {
        one(workspace).projects.willReturnIterable(
          {p: Project => one(p).name willReturn "p1" },
          {p: Project => one(p).name willReturn "p2" })
      }
      workspace.projects.map(_.name) must_== List("p1", "p2")
    }
    "provide a will method to add any action to an expectation" in {
      expect {
        one(list).get(anyInt) will(returnValue("hey"))
      }
      list.get(0) must_== "hey"
    }
    "provide a will method to return different values on consecutive calls" in {
      expect {
        1.atLeastOf(list).get(anyInt) willReturnEach ("a", "b", "c")
      }
      ("a", "b", "c") foreach { list.get(0) must_== _}
    }
    "provide a then method to constraint calls to occur in sequence" in {
      expect {
        one(list).size then
        one(list).get(anyInt) then
        one(list).isEmpty
      }
      list.size
      list.get(0)
      list.isEmpty
    }
    "provide a when/set methods to constraint calls to occur in specific states" in {
      val readiness = state("readiness")
      readiness.startsAs("not ready")
      expect {
        one(list).size set readiness.is("ready")
        allowing(list).get(anyInt) when readiness.is("ready")
      }
      list.size
      list.get(0)
    }
    "provide a isExpectation method to register an additional expectation when a mock is called" in {
      object mockSpec extends Specification with MockOutput with JMocker {
        "spec with mock expectations" in {
          expect { one(list).get(anyInt).isExpectation }
        }
      }
      mockSpec.expectationsNb must_== 2 // one for the whole expect block, one for the inside expectation
    }
    "provide a one-liner expression for expectations" in {
      expect[ToMock] { one(_).isEmpty } in { _.isEmpty }
    }
    "provide a one-liner expression for expectations, adding an 'expects' method on Classes returning a mock" in {
      classOf[ToMock].expects(one(_).isEmpty willReturn false) in { _.isEmpty must beFalse}
    }
    "provide a one-liner expression for expectations, adding an 'expectsOne' method on Classes returning a mock" in {
      classOf[ToMock].expectsOne(_.isEmpty) in { _.isEmpty }
    }
    "provide a one-liner expression for expectations, adding an 'neverExpects' method on Classes returning a mock" in {
      classOf[ToMock].neverExpects(_.isEmpty) in { (m: ToMock) => }
    }
    "provide a one-liner expression for expectations, adding an 'isIgnored' method on Classes returning a mock" in {
      classOf[ToMock].isIgnored in { _.isEmpty2 }
    }
    "provide a one-liner expression for expectations, returning the mock object with 'mock'" in {
      val mock = classOf[ToMock].expectsOne(_.isEmpty).mock
      mock.isEmpty
    }
    "provide a willReturn method returning the value captured by a parameter as a return value action" in {
      val s = capturingParam[String]
      classOf[ToMock].expects(one(_).method0(s.capture) willReturn s) in {
        _.method0("b") must_== "b"
      }
    }
    "provide a willReturn method returning the value captured by the second parameter of a method" in {
      val s = capturingParam[String]
      classOf[ToMock].expects(one(_).method1(anyString, s.capture(1)) willReturn s) in {
        _.method1("a", "b") must_== "b"
      }
    }
    "provide a willReturn method returning the value captured by a parameter mapped with another function" in {
      val s = capturingParam[String]
      classOf[ToMock].expects(one(_).method2(s.capture) willReturn s.map(_.size + 1)) in {
        _.method2("b") must_== 2
      }
    }
    "provide a willReturn method returning the value captured by a parameter, but checked with a matcher before" in {
      val s = capturingParam[String]
      classOf[ToMock].expects(one(_).method0(s.must(beMatching("b")).capture) willReturn s) in {
        _.method0("b")
      }
    }
  }
}

object jmockBadSpecification extends BadMocked {
  "The JMocker trait" should { 
    "provide a 'one' method failing if no method is called" in {
       expect { one(list).size }
       
       1 must_== 1
    }
    "provide an 'exactly' method failing if a lesser number of calls are made" in {
       expect { exactly(2).of(list).size }
       list.size
    }
    "provide an 'exactly' method failing if a bigger number of calls are made" in {
       expect { exactly(2).of(list).size }
       3 times {i => list.size}
    }
    "provide an 'atLeast' method failing if less than the right number of calls are made" in {
       expect { atLeast(2).of(list).size }
       list.size
    }
    "provide a 'between' method failing if the number of calls is below a range" in {
       expect { between(2, 4).of(list).size }
       list.size
    }
    "provide a 'between' method failing if the number of calls is above a range" in {
       expect { between(1, 2).of(list).size }
       3 times {i => list.size()}
    }
    "provide an 'atMost' method failing if more calls are being made" in {
       expect { atMost(2).of(list).size }
       3 times {i => list.size()}
    }
    "provide an 'allowing' method failing if calls dont match a method pattern" in {
       expect { allowing(anything).method(withName("toSize")) }
       list.size
    }
    "provide a 'never' method failing if any call is made to the mock" in {
       expect { never(list) }
       list.size
    }
    "provide an equal matcher failing if the passed parameter is not equal to the specified one" in {
      expect { 1.of(list).get(equal(0)) }
      list.get(1)
    }
    "provide a then method failing if calls are not made in sequence" in {
      expect {
        one(list).size then
        one(list).get(anyInt) then
        one(list).isEmpty
      }
      list.size
      list.isEmpty
      list.get(0)
    }
    "provide a when/set methods failing if method calls don't occur in proper states" in {
      val readiness = state("readiness")
      readiness.startsAs("not ready")
      expect {
        allowing(list).get(anyInt) when readiness.is("ready")
      }
      list.get(0)
    }
  }
}
class BadMocked extends Mocked {
  var checkAfterExpectations = true
  override def executeExpectations(ex: Examples, t: => Any) = {
    try {
      setCurrent(Some(ex))
      t
    } catch {
      case e: org.jmock.api.ExpectationError => {checkAfterExpectations = false}
    }
  }
  override def afterExpectations(ex: Examples) = {
    if (checkAfterExpectations)
      try { context.assertIsSatisfied } catch {
        case e: org.jmock.api.ExpectationError =>
        case _ => ex.addFailure(new org.specs.execute.FailureException("Expected a org.jmock.api.ExpectationError, got nothing"))
      }
      else
        checkAfterExpectations = true
  }
}
class Mocked extends Specification with JMocker with ClassMocker {
  class ToMock {
    def isEmpty = true
    def isEmpty2 = false
    def method0(p1: String) = p1
    def method1(p1: String, p2: String) = p1
    def method2(p1: String) = p1.size
    def method3(p1: String) = List(p1)
    def methodWithLazy(p1: =>String) = p1
  }

  val list: java.util.List[Object] = mock[java.util.List[Object]]
  val scalaList: List[String] = mockAs[List[String]]("scalaList")
}

