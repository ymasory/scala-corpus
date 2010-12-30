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
import org.specs._
import org.specs.specification._
import org.specs.util.Property
import scala.xml._

trait RunnerFixture extends RunnerTestData with literate.Html { this: HtmlSpecificationWithJUnit =>
  def createSimpleSpecRunner = runner = simpleSpecRunner
  def executeCompositeSpecRunner = { runner = compositeSpecRunner; executeRunner }
  def executeRunner = { runner.reset; runner.reportSpecs.shh }
  def runnerOutputDir = { runner.setOutputDir _ }
  def checkXml = XML.loadString(runner.readFile(runner.files.keysIterator.next)) must \\(xml())

  def checkFilePath = {
    createSimpleSpecRunner
    executeRunner
    runner.files must haveKey(path())
  }
  def checkOutputDirectory = {
    runner.reset
    executeRunner
    runner.files must haveKey(path.toString)
  }
  def checkConsole = simpleSpecRunner.messages must not(beEmpty)
}
trait RunnerTestData {
  import org.specs.io.mock._
  import org.specs.io._
  var path = Property("")
  var xml: Property[Elem] = Property(<p></p>).onToString(e => new PrettyPrinter(200, 2).format(e))
  trait OutputDirSettable extends File {
    var dirPath: String = ""
    def setOutputDir(dir: String) = dirPath = dir
    override def outputDir = dirPath
  }
  var runner: XmlRunner with Console with MockFileSystem with OutputDirSettable = _
  object simpleSpecRunner extends XmlRunner(sp1) with Console with MockFileSystem with MockOutput with OutputDirSettable
  object compositeSpecRunner extends XmlRunner(compositeSpec) with Console with MockFileSystem with MockOutput with OutputDirSettable
  object compositeSpec extends Specification {
    "a composite spec" isSpecifiedBy(sp1, sp1)
  }
}

object sp1 extends Specification {
  override def toString = name
  "the sus" should {
    "have one ok example" in { 1 must_== 1 }
    "have one ko example" in { 1 mustBe 2 }
    "have an example with an error" in { throw new Error("error message") }
    "have one sub-example" in { "a sub-example" in { 1 must_== 1 } }
  }
}
