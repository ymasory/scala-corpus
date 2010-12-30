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
import org.specs.io.mock.{ MockOutput, MockFileSystem }
import org.specs.io.ConsoleOutput
import org.specs.runner._
import org.specs._

class specsFinderSpec extends SpecificationWithJUnit with Init {
  val finder = new MockFileSystem with SpecificationsFinder with MockOutput
  "A specs finder" should { doBefore(finder.reset)
    finder.defaultExtension = ".scala"
    "not find the name of a specification in the file is not a .scala file" in {
      finder.addFile("fileName", packageDeclaration + specificationContent)
      finder.specificationNames("path", ".*") mustBe Nil
    }
    "find the name of a specification if it is matched by a user-defined pattern" in {
      finder.addFile(packageDeclaration + specificationContent)
      finder.specificationNames("path", "trueSpec") mustContain "org.specs.trueSpec$"
      finder.specificationNames("path", "tr.*Spec") mustContain "org.specs.trueSpec$"
      finder.specificationNames("path", "gloups.*Spec") mustNotContain "org.specs.trueSpec$"
    }
    "find the name of a specification in a specification file" in {
       finder.addFile(packageDeclaration + specificationContent)
       finder.specificationNames("path", ".*") mustContainMatch "trueSpec"
    }
    "create the name of a specification with its package name" in {
      finder.addFile(packageDeclaration + specificationContent)
      finder.specificationNames("path", ".*") mustContain "org.specs.trueSpec$"
    }
    "create the name of a specification with its package name if the package declaration ends with a;" in {
      finder.addFile(packageDeclarationWithSc + specificationContent)
      finder.specificationNames("path", ".*") mustContain "org.specs.trueSpec$"
    }
    "create the name of a specification with no package name for a file with no package declaration" in {
      finder.addFile(specificationContent)
      finder.specificationNames("path", ".*") mustContain "trueSpec$"
    }
    "create the name of a specification if it extends another specification class" in {
      finder.addFile(packageDeclaration + "\nobject mySpec extends Parent")
      finder.specificationNames("path", ".*") mustContain "org.specs.mySpec$"
    }
    "create the name of a specification if it is a class" in {
      finder.addFile(packageDeclaration + "\nclass mySpec extends Parent")
      finder.specificationNames("path", ".*") mustContain "org.specs.mySpec"
    }
    "return an empty list if there is no specification declaration found in the file" in {
      finder.addFile(packageDeclaration)
      finder.specificationNames("path", ".*") must_== List()
    }
    "return a list with 2 specification names if the file contains 2 specs" in {
      finder.addFile(packageDeclaration + specificationContent + specificationContent)
      finder.specificationNames("path", ".*") must_== List("org.specs.trueSpec$", "org.specs.trueSpec$")
    }
    "return a list with 2 specification names if run over a directory with 2 files" in {
      finder.addFile("file1.scala", packageDeclaration + specificationContent)
      finder.addFile("file2.scala", packageDeclaration + specificationContent)
      finder.specificationNames("dir1", ".*") must_== List("org.specs.trueSpec$", "org.specs.trueSpec$")
    }
    "not fail when trying to create a spec for an object not inheriting the specification class" in {
      finder.createSpecification("org.specs.runner.NotASpecification") must beNone
    }
    "print an error message when failing load a spec class when the -DdebugLoadClass option is true" in {
      System.setProperty("debugLoadClass", "true")
	  finder.createSpecification("org.specs.runner.NotASpecification", true, false)
      finder.messages must containMatch("is not an instance of")
    }
  }
}
class NotASpecification
trait Init {
  val packageDeclaration = "package org.specs"
  val packageDeclarationWithSc = packageDeclaration + ";"
  val specificationContent = """
    object trueSpec extends Specification with MockOutput {
      "A specification" should {
        "have example 1 ok" in { true }
        }
      }
    """
}

