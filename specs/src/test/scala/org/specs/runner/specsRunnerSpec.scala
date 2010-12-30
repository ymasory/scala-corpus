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
import org.specs.util._
import scala.collection.mutable.Queue
import org.specs.io.mock.MockOutput
import org.specs.runner._
import org.specs.specification._

class specsRunnerSpec extends SpecificationWithJUnit with TestRunner {
  "A specs file runner" should { runner.clearMessages.before
    "execute a specification contained in a file" in {
      runTheFileWithClassName("org.specs.samples.sampleSpecification1$")
      messages mustHaveMatch "example"
    }
    "execute 2 specifications contained in a directory" in {
      runTheFileWithClassName("org.specs.samples.sampleSpecification1$", "org.specs.samples.sampleSpecification2$")
      messages mustHaveMatch "specification1"
      messages mustHaveMatch "specification2"
    }
  }
}
trait MockSpecsFinder extends SpecsFinder {
  var classNames: List[String] = Nil
  override def specificationNames(filesPath: String, pattern: String) = classNames
}

trait TestRunner {
  class SpecsRunner extends SpecsFileRunner("", ".*") with MockSpecsFinder with MockOutput
  var runner: SpecsRunner = new SpecsRunner
  def runTheFileWithClassName(classNames: String*) = {
    runner = new SpecsRunner
    runner.classNames = classNames.toList
    runner.reportSpecs
  }
  def messages = runner.messages
}

object AllSpecsFileRunner extends SpecsFileRunner("./src/test/scala/org/specs", "([^(?>all)].)*Spec")
object AllUnitFileRunner extends SpecsFileRunner("./src/test/scala/org/specs", "([^(?>all)].)*Unit")
object AllFileRunner extends SpecsFileRunner("./src/test/scala/org/specs", "([^(?>all)].)*.*")


