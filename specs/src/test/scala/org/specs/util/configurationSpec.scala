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
package org.specs.util
import org.specs.io.mock._

class configurationSpec extends org.spex.Specification {
  "A configuration object" should {
    "try to find the default configuration class, named DefaultConfiguration, in the default package" in {
      Configuration.getDefaultConfiguration must haveClass[DefaultConfiguration]
    }
    "try to find first the user configuration class, named configuration$ (an object), in the default package" +
    "defaulting to the default configuration if not found" in {
      Configuration.getUserConfiguration must haveClass[DefaultConfiguration]
    }
    "try to find a configuration class, with a given name defaulting to the user configuration" +
    "then defaulting to the DefaultConfiguration if the user config is not found"  in {
      Configuration.getConfiguration("missing") must haveClass[DefaultConfiguration]
    }
    "try to find a configuration class, with a given class name defaulting to the user configuration" in {
      Configuration.getConfiguration("org.specs.util.TestConfiguration") must haveClass[TestConfiguration]
    }
    "try to find a configuration properties file and load the properties from there" in {
      val props = """"
stacktrace = false
failedAndErrorsOnly = true
statistics = false
finalStatisticsOnly = true
colorize = true
examplesWithoutExpectationsMustBePending = false
oneSpecInstancePerExample = true
smartDiffs = false
"""
      configuration.addFile("configuration.properties", props)
      val c = configuration.getConfiguration("configuration.properties")
      (c.stacktrace, c.statistics, c.examplesWithoutExpectationsMustBePending, c.smartDiffs).foreach(_ must beFalse)
      (c.failedAndErrorsOnly, c.finalStatisticsOnly, c.colorize, c.oneSpecInstancePerExample).foreach(_ must beTrue)
    }
  }
  "A configuration" can {
   "translate boolean properties from a properties file" in {
     val properties = new java.util.Properties
     properties.put("stacktrace", "true")
     "a missing name returns the default value" in {
       Configuration.boolean(properties, "missing", true) must beTrue
       Configuration.boolean(properties, "missing", false) must beFalse
     }
     "a existing name returns the file value" >> {
       Configuration.boolean(properties, "stacktrace", true) must beTrue
       Configuration.boolean(properties, "stacktrace", false) must beTrue
     }
     "a value of y, yes, Y, yes is taken as true" >> {
       List("y", "yes", "Y", "Yes").foreach {  (s: String) =>
         properties.put("stacktrace", s)
         Configuration.boolean(properties, "stacktrace", false) must beTrue
       }
     }
     "a value of n, no, N, No is taken as false" >> { 
       List("n", "no", "N", "No").foreach { (s: String) =>
         properties.put("stacktrace", s)
         Configuration.boolean(properties, "stacktrace", true) must beFalse
       }
     }
    }
  }
  val configuration = new Configuration with MockFileSystem
}
class TestConfiguration extends Configuration {
  override def finalStatisticsOnly = true
  override def failedAndErrorsOnly = true
}
