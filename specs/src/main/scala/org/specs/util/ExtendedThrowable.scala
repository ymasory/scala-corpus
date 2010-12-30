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
import org.specs.util.ExtendedString._
import java.io.StringWriter
import java.io.PrintWriter

/**
 * This object allows to add some utility methods to </code>Throwable</code> objects.
 */
object ExtendedThrowable {
  /**
   * Implicit method to add additional methods to Throwable objects
   */
  implicit def toExtendedThrowable[T <: Throwable](t: T) = new ExtendedThrowable(t)  
  /**
   * See the ExtendedThrowable object description
   */
  class ExtendedThrowable[T <: Throwable](t: T) {
    private def fileName = t.getStackTrace()(0).getFileName
    private def className = t.getStackTrace()(0).getClassName.removeFrom("$")
    private def lineNumber = t.getStackTrace()(0).getLineNumber

    /** @return the file name and the line number where the Throwable was created */
    def location: String = fileName + ":" + lineNumber
    /** @return the class name and the line number where the Throwable was created */
    def classLocation: String = className + ":" + lineNumber
    /** @return the class name, file Name and the line number where the Throwable was created */
    def fullLocation: String = className + " (" + location + ")"
    /** @return the stack trace as a string with where each message is separated by a new line */
    def stackToString: String = stackToString("", "\n", "\n")
    /** @return the stack trace with user-specified separators */
    def stackToString(first: String, separator: String, last: String): String = t.getStackTrace.mkString(first, separator, last)
    /** @return stack trace written using <code>Throwable.printStackTrace()</code> */
    def printStackTraceToString = {
      val w = new StringWriter
      t.printStackTrace(new PrintWriter(w))
      w.toString
    }
    /** 
     * remove all traces of this exception until the last line matching <code>name</code> is found.
     */
    def removeTracesAsFarAsNameMatches(name: String): Throwable = {
      t.setStackTrace(t.getStackTrace.toList.drop(1).reverse.takeWhile { x: StackTraceElement => 
                             !x.toString.matches(".*" + name + ".*") }
                      .reverse.toArray)
      t
    }
    /** 
     * remove all traces of this exception until there's a line not matching <code>name</code>.
     */
    def removeTracesWhileNameMatches(name: String): Throwable = {
      t.setStackTrace((t.getStackTrace.toList.drop(1).dropWhile { x: StackTraceElement => 
                             x.toString.matches(".*" + name + ".*") 
                          }).toArray)
      t
    }
    /**
     * throw an exception removing all the stack trace elements matching the class name of the caller.
     * @param caller object used to define the elements to remove 
     */
    def hideCallerAndThrow(caller: Object) = {
      throw removeTracesWhileNameMatches(getClassName(caller))
    }
    def hideCallerAndThrow(caller: String) = {
      throw removeTracesWhileNameMatches(caller)
    }
    /**
     * throw an exception using the stacktrace of another one.
     * @param other other exception whose stacktrace should be used 
     */
    def throwWithStackTraceOf(other: Throwable) = throw t.setAs(other)
    /**
     * set an exception with the stacktrace of another one.
     * @param other other exception whose stacktrace should be used 
     */
    def setAs(other: Throwable): T = {
      t.setStackTrace(other.getStackTrace)
      t.initCause(other.getCause)
      t
    }
	/**
	 * @return the stack trace elements of all the chained exceptions
	 */
	 def getFullStackTrace: List[java.lang.StackTraceElement] = {
	   (t :: t.chainedExceptions).flatMap(_.getStackTrace.toList)
	 }
	/**
	 * @return the list of chained exceptions
	 */
	def chainedExceptions: List[Throwable] = {
	  if (t.getCause == null) Nil
	  else t.getCause :: t.getCause.chainedExceptions
	}
    /**
     * @return the class name of an object without $.
     */
    private def getClassName(o: Object) = o.getClass.getName.split("\\.").last.replace("$", "")
  }
}
