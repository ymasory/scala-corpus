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
import org.specs.runner._
import org.specs.Sugar._
import org.specs.specification._
import org.specs.util.ExtendedThrowable._
import org.specs._

class extendedThrowableUnit extends SpecificationWithJUnit with ExceptionSamples {

  "an extended Throwable with location methods" ->- ex should provide {
    "a location method extracting the name of the file and the line from an exception" in {
      e.location must_== "extendedThrowableUnit.scala:12"
    }
    "a class location method extracting the class name and line number of an exception" in {
      e.classLocation must_== "org.specs.extendedThrowableUnit:12"
    }
    "a class location method extracting the class name and line number of an exception" in {
      e.classLocation must_== "org.specs.extendedThrowableUnit:12"
    }
  }
  "an extended Throwable with string methods" ->- ex should provide {
    "a stackToString method returning a displayable string of the stack trace elements" in {
      e.stackToString must startWith("org.specs.extendedThrowableUnit$.apply(extendedThrowableUnit.scala:12)\n" +
                                     "org.specs.extendedThrowableUnit$.apply(extendedThrowableUnit.scala:13)\n")
    }
    "a stackToString method returning a displayable string of the stack trace elements, with user-selected separators" in {
      e.stackToString("====", "|", "++++") must (startWith("====") and include("|") and endWith("++++"))
    }
  }
  "an extended Throwable with stack traces methods" ->- ex should provide {
    "a removeTracesWhileNameMatches function removing stacktraces until a line doesn't match the name" in {
      e.removeTracesWhileNameMatches("extendedThrowableUnit").getStackTrace.toList.head.toString aka
      "the first element of the remaining stack" must_== "org.specs.specification.method0(Specification.scala:23)"
    }
    "a removeTracesAsFarAsNameMatches function removing stacktraces until the last match with a name is found" in {
      e.removeTracesAsFarAsNameMatches("extendedThrowableUnit").getStackTrace.toList.head.toString aka
      "the first element of the remaining stack" must_== "org.specs.specification.method1(Specification.scala:24)"
    }
    "a hideCallerAndThrow method to throw the exception but removing stack trace elements having the caller class name" in {
      try { e.hideCallerAndThrow(this) }
      catch {
        case ex => ex.getStackTrace.toList.head.toString aka
                   "the first element of the thrown exception" must_== "org.specs.specification.method0(Specification.scala:23)"
      }
    }
    "a throwWithStackTraceOf method to throw the exception with another exception stacktrace" in {
      try { e.throwWithStackTraceOf(e2) }
      catch {
        case ex =>
               ex.getMessage must_== "failure"
               ex.getStackTrace.toList.head.toString aka
               "the first element of the thrown exception" must_== "org.specs.Specification.apply(Specification.scala:5)"
      }
    }
  }
  "an extended Throwable" should {
    "return the full stacktrace of an exception including all causes recursively" in {
	  val cause2 = new Exception("cause2")
	  val cause1 = new Exception("cause1", cause2)
	  val e = new Exception("root", cause1)
	  val fullStack = e.getFullStackTrace.map(_.toString).filter(_.contains("JUnitSuiteRunner"))
	  fullStack.size must_== 3
	}
  }
  def provide = addToSusVerb("provide")
}
trait ExceptionSamples extends Contexts { this: BaseSpecification => 
  var e: Exception = _
  var e2: Exception = _
  val ex = beforeContext {
    e = createException
    e2 = createException2
  }
  def createException = exception("failure", ("org.specs.extendedThrowableUnit$", "apply", "extendedThrowableUnit.scala", 12),
                    ("org.specs.extendedThrowableUnit$", "apply", "extendedThrowableUnit.scala", 13),
                    ("org.specs.specification", "method0", "Specification.scala", 23),
                    ("org.specs.extendedThrowableUnit", "test", "extendedThrowableUnit.scala", 15),
                    ("org.specs.specification", "method1", "Specification.scala", 24)
                    )
  def createException2 = exception("failure2", ("org.specs.Specification", "apply", "Specification.scala", 5),
                    ("org.specs.Specification", "apply", "Specification.scala", 10),
                    ("org.specs.specification.Expectable", "method0", "Expectable.scala", 18)
                    )
  def exception(name: String, trace: (String, String, String, Int)*) = {
    val e = new Exception(name)
    e.setStackTrace(trace.map { t =>
      var (className, method, file, line) = t
      new java.lang.StackTraceElement(className, method, file, line)}.toArray)
    e
  }
}
