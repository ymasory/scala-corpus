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
package org.specs.samples
import org.specs.runner._
import org.specs._

object runners {
  object spec extends Specification
  object spec2 extends Specification
  // those are different possibilities of runners combinations for specs
  // 1. run a spec in the console -- this is actually redundant as specification are automatically runnable with
  //    an embedded ConsoleRunner
  object runner1 extends ConsoleRunner(spec)
  // 2. run a spec with JUnit4. runner2 needs to be a class
  class runner2 extends JUnit4(spec)
  // 3. run a spec as an xml file (created in the 'target' directory)
  object runner3 extends XmlRunner(spec, "target")
  // 4. run a spec as an html file (created in the 'target' directory)
  object runner4 extends HtmlRunner(spec, "target")
  // 5. run a spec as an html file and in the console
  object runner5 extends HtmlRunner(spec, "target") with Console
  // 6. run a spec as an xml + html file + in the console
  object runner6 extends Runner(spec :: spec2, runner3 :: runner4) with Console
  // 8. run 2 specs as an xml + html file + in the console
  object runner8 extends Runner(spec, spec2) with Console
  // 9. run all specifications found on a given path as an xml + html file + in the console
  object runner9 extends Runner(SpecsFinder("src/test", ".*Spec.*"), new XmlRunner :: new HtmlRunner) with Console with JUnit
}
class spec10 extends Specification with ScalaTest {
  1 must_== 1
  2 must_== 2
}
class spec11Test extends SpecificationWithJUnit with ScalaTest {
  1 must_== 1
  2 must_== 2
}