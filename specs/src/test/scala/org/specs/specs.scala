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
package org.specs
import org.specs.runner._
import org.specs.literate._
import org.specs.matcher._
import org.specs.samples._
import org.specs.mock._
import org.specs.form._
import org.specs.specification._
import org.specs._
import org.specs.io._
import org.specs.collection._
import org.specs.util._
import org.specs.xml._
import org.specs.execute._

object allSpecifications extends Specification {
  "The specifications" areSpecifiedBy (
    executeSpecifications,
    formSpecifications,
    ioSpecifications,
    literateSpecifications,
    matcherSpecifications,
    mockSpecifications,
    samplesSpecifications,
	specificationSpecifications,
    runnerSpecifications,
    utilSpecifications,
    xmlSpecifications)
}

object allUnits extends Specification {
  "The unit tests" areSpecifiedBy (
	collectionUnits,
	formUnits,
	ioUnits,
    literateUnits,
	matcherUnits,
	mockUnits,
	runnerUnits,
	specificationUnits,
	utilUnits,
	xmlUnits)
}

object allSpecsAndUnits extends Specification {
  "The specs and unit tests for the specs project" areSpecifiedBy (allSpecifications, allUnits)
}
class allSuite extends JUnit4(allSpecsAndUnits)
object allRunner extends Runner(allSpecsAndUnits) with ScalaTest with JUnit
object allXml extends XmlRunner(allSpecsAndUnits)
