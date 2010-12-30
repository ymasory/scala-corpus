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
import org.specs.Sugar._
import org.specs.mock._
import org.specs._

class mockerUnit extends SpecificationWithJUnit with Sugar with ProtocolTypes {
  val mocker = new Mocker { def addExpectation = null }
  class MockedClass { def method = (); def a = (); def b = (); def c = () }
  "A mocker" should {
    "create a protocol when expecting calls" in {
      val protocol = mocker.expect {}
      protocol mustNotBe null
    }
    "add a new expected call when recording calls" in {
      val mock = new MockedClass() { override def method = mocker.record }
      val protocol = mocker.expect { mock.method }

      protocol verifies(_.isSpecified)
      protocol.definition mustNotBe null
      protocol.definition.expectedCalls must beLike { case List(ExpectedCall(_)) => ok }
    }
    "add a new received call when receiving calls" in {
      val mock = new MockedClass() { override def method = mocker.record }
      val protocol = mocker.expect { mock.method }
      mock.method

      protocol.receivedCalls must beLike { case List(ReceivedCall(_)) => ok }
    }
    "have a failure when not receiving an expected call" in {
      val mock = new MockedClass() { override def method = mocker.record }
      val protocol = mocker.expect(inAnyOrder) { mock.method }

      protocol.failures must beMatching("Expected in any order .*. Received none")
    }
    "include a protocol def inside a protocol def if expectations are nested" in {
      val mock = new MockedClass() { override def a = mocker.record; override def b = mocker.record; override def c = mocker.record }
      val protocol = mocker.expect {
        mock.a
        mocker.expect { mock.b }
        mock.c
      }
      protocol.definition must beLike { case ProtocolDef(inAnyOrder, List(ExpectedCall(_),
                                                                           ProtocolDef(_, _),
                                                                           ExpectedCall(_))) => ok }
    }
    "accept nested protocol defs using different protocol types: anyOf 1.of{method}; 2.of {method}" in {
      val mock = new MockedClass() { override def a = mocker.record; override def b = mocker.record; override def c = mocker.record }
      val protocol = mocker.expect {
        mocker.expect(oneOf) { mock.a }
        mocker.expect(twoOf) { mock.b }
      }
      protocol.definition must beLike { case ProtocolDef(inAnyOrder, List(ProtocolDef(x, _),
                                                                          ProtocolDef(y, _))) => (x, y) == (oneOf, twoOf) }
    }
  }
}
