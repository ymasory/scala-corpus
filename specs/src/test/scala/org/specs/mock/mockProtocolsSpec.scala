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
import org.specs.matcher._
import org.specs.Sugar._
import org.specs.specification.{ Example, Sus }
class mockProtocolsSpec extends MatchersSpecification with ButtonAndLightMock {
  "Mock protocols" should {
   "provide an 'expect oneOf' protocol checking if one call exactly has been made" in {
     var protocol = expect(oneOf) { mock.on; mock.off }
     expectation(protocol must beMet) must failWithMatch("Expected in any order \\[on\\(.*\\); off\\(.*\\)\\]. Received none")
     protocol.clear

     protocol = expect(oneOf) { mock.on; mock.off }
     2.times {i => button.push}
     protocol must beMet
     protocol.clear

     protocol = expect(oneOf, exclusively) { mock.on; mock.off }
     3.times {i => button.push}
     expectation(protocol must beMet) must failWithMatch("Expected in any order \\[on\\(.*\\); off\\(.*\\)\\]. Received:\n  on\\(.*\\)\n  off\\(.*\\)\n  on\\(.*\\)")
     protocol.clear
  }

  "provide an 'expect inAnyOrder' protocol checking if calls have been made to mock objects" in {
      // by default, the calls can be made in any order
      val protocol = expect(inAnyOrder){mock.on; mock.off}
      expectation(protocol must beMet) must (failWithMatch("Expected in any order \\[on\\(.*\\); off\\(.*\\)\\]"))

      button.push
      expectation(protocol must beMet) must failWithMatch("Expected in any order \\[on\\(.*\\); off\\(.*\\)\\]")

      button.push  // the protocol is always checked at the end of an example
   }
   "provide an 'expect exclusively' argument checking if more calls have been made" in {
      // by default, the calls can be made in any order
      val protocol = expect(inAnyOrder, exclusively) { mock.on; mock.off}
      expectation(protocol must beMet) must (failWithMatch("Expected in any order \\[on\\(.*\\); off\\(.*\\)\\]"))

      3.times {i=> button.push}
      expectation(protocol must beMet) must failWithMatch("Expected in any order \\[on\\(.*\\); off\\(.*\\)\\]")

      protocol.clear
    }
   "provide an 'expect anyOf' protocol authorizing any of a set of calls to be made" in {
     var protocol = expect(anyOf) { mock.destroy.isExpectation }
     button.pound
   }
  "provide an 'expect n.of' protocol checking if exactly n calls have been made" in {
     var protocol = expect(2.of) { mock.on; mock.off }
     expectation(protocol must beMet) must failWithMatch("Expected 2 of: \\[on\\(.*\\); off\\(.*\\)\\]. Received none")

     protocol = expect(2.of) { mock.on; mock.off }
     4.times {i => button.push}
   }
   "provide an 'expect atLeastOneOf' protocol checking if at least one call has been made" in {
     var protocol = expect(atLeastOneOf) { mock.on; mock.off }
     expectation(protocol must beMet) must failWithMatch("Expected at least 1 of: \\[on\\(.*\\); off\\(.*\\)\\]. Received none")

     protocol = expect(atLeastOneOf) { mock.on; mock.off }
     2.times {i => button.push}
   }
   "provide an 'expect at least n of' protocol checking if exactly n calls have been made" in {
     var protocol = expect(3.atLeastOf) { mock.on; mock.off }
     expectation(protocol must beMet) must failWithMatch("Expected at least 3 of: \\[on\\(.*\\); off\\(.*\\)\\]. Received none")

     protocol = expect(2.atLeastOf) { mock.on; mock.off }
     5.times {i => button.push}
   }
   "provide an 'expect inSequence' protocol checking if calls have been made to mock objects inSequence" in {
      var protocol = expect(inSequence) { mock.off; mock.on }
      2.times {i => button.push}
      expectation(protocol must beMet) must failWithMatch("Expected in sequence \\[off\\(.*\\); on\\(.*\\)\\]. Received:\n  on\\(.*\\)\n  off\\(.*\\)")

      protocol = expect(inSequence) { mock.on; mock.off }
      2.times {i => button.push}
    }
  }
  "Mock protocols" can {
     "be nested to allow complex expectations: expect, inAnyOrder 1 'on' and 2 'off'" in {
     val protocol = expect(inAnyOrder) {
        expect(oneOf){mock.on; mock.off; mock.on}
        expect(oneOf){mock.off}
      }

      expectation(protocol must beMet) must failWithMatch(".*Expected in any order \\[in any order \\[on\\(.*\\); off\\(.*\\); on\\(.*\\)]; in any order \\[off\\(.*\\)\\]\\]. Received none")

      2.times {i => button.push}
      expectation(protocol must beMet) must failWithMatch(".*Expected in any order \\[in any order \\[on\\(.*\\); off\\(.*\\); on\\(.*\\)\\]; in any order \\[off\\(.*\\)\\]\\]. Received.*")

      protocol.clear
      button.init()
      2.times {i => button.push}
    }

  }
}
trait ButtonAndLightMock extends ButtonAndLight with Mocker {
  val mock = new Light {
    override def on = record
    override def off = record
    override def destroy = record
  }
  val button = Button(mock)
}
trait ButtonAndLight {
  case class Button(light: Light) {
    var lightOn = false
    def push = {
      if (lightOn) light.off else light.on
      lightOn = !lightOn
    }
    def pound = light.destroy
    def init() = lightOn = false
  }
  case class Light() {
    var state: LightState = Off
    def on = state = On
    def off = state = Off
    def isOn = state == On
    def destroy = {}
  }
  implicit val li = Light()

  abstract sealed class LightState(s: String)
  object On extends LightState("on")
  object Off extends LightState("off")
}

