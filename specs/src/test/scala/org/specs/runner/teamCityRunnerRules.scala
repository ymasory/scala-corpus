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
package org.specs.runner
import org.specs.specification._
import org.specs.io.mock._
import org.specs.util.Property
import org.specs._

class teamCityRunnerRules extends HtmlSpecificationWithJUnit("Team city runner") {
  override def htmlDir = "target"
  def clearDetails(messages: Seq[String]) = messages.map(_.replaceAll("details='.*'", "details='exception stacktrace'"))
  val message: Property[String] = Property("")
  val messages: Property[List[String]] = Property[List[String]](Nil)
  def messageMustBeCreated = clearDetails(runSpec.messages) must contain(message())
  def messagesMustBeCreated = clearDetails(runSpec.messages) must containInOrder(messages())
  def runSpec = (new TeamCityRunner(testingSpecification) with MockOutput).reportSpecs
} 

object testingSpecification extends Specification("specification name") {
  "sus1 description" should {
    "good example" in { true must beTrue }
    "failed example" in { true must beFalse }
    "exception example" in { throw new Error("error") }
    "sub examples" in {
      "good sub" in { true must beTrue }
      "bad sub1" in { true must beFalse }
      "bad sub2" in { false must beTrue }
    }
  }
}
