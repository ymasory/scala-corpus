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
import org.specs.util.DataTables
import org.specs.Sugar._
import org.specs._

class xmlRunnerUnit extends SpecificationWithJUnit with DataTables with Html {
  override def htmlDir = "target"
  "An xml runner" should {
    "create an xml file in the default directory if nothing is specified" in {
       xRunner.reportSpecs
       xRunner.files must haveKey("./spec1.xml")
    }
    "create an xml file in the specified output directory, handling file separators" in {
       "output dir" | 	"spec name" | 	"file path"  				|>
       "" 		    ! 	"spec1" 	!	"./spec1.xml"				|
       "result" 	!	"spec1" 	!	"./result/spec1.xml" 		|
       "result/" 	!	"spec1" 	!	"./result/spec1.xml" 		|
       "result\\" 	!	"spec1" 	!	"./result/spec1.xml" 		|
       "/result" 	!	"spec1" 	!	"/result/spec1.xml" 		|
       "\\result" 	!	"spec1" 	!	"/result/spec1.xml" 		|
       "result/xml" ! 	"spec1"     !	"./result/xml/spec1.xml"	| { (dir, spec, result) =>
           xRunner.reset
           xRunner.setOutputDir(dir)
           spec1.name = spec
           xRunner.reportSpecs
           xRunner.files.keySet must contain(result)
       }
    }
  }
}
import org.specs.io.mock._
trait ExtendedMockFileSystem extends MockFileSystem {
  override def createFile(path: String) = {files += (path -> ""); true}
}
object xRunner extends XmlRunner(spec1, "result", XmlNamingFunction.short) with ExtendedMockFileSystem with Console with MockOutput {
  var dirPath = ""
  def setOutputDir(dir: String) = dirPath = dir
  override def outputDir = dirPath
}
object spec1 extends Specification {
  "the sus" should { "have one ok example" in { 1 mustBe 1 } }
}
