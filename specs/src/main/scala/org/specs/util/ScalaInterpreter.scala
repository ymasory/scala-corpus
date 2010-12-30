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
import scala.tools.nsc.{ Interpreter, Settings, InterpreterResults }
import java.io.{ PrintWriter, StringWriter, File }
import java.net.{ URLClassLoader, URLDecoder }

trait ScalaInterpreter {
  private val writer = new StringWriter
  private val interpreter = createInterpreter
  private def createInterpreter = {
    val settings = new Settings
    var cl = Thread.currentThread.getContextClassLoader
    var paths = new scala.collection.mutable.ListBuffer[Any]
    if (cl.isInstanceOf[URLClassLoader]) {
      val ucl = cl.asInstanceOf[URLClassLoader]
      ucl.getURLs.foreach { url =>
        if (url.getProtocol.equals("file")) {
          paths.append(new File(URLDecoder.decode(url.getPath, "UTF-8")).getCanonicalFile.getAbsolutePath) 
        }
      }
    }
    settings.classpath.value = (List(settings.classpath.value) ::: paths.toList).mkString(File.pathSeparator)
    settings.usejavacp.value = true
    new Interpreter(settings, new PrintWriter(writer))
  }
  private def clear(writer: StringWriter) = {
    val buf = writer.getBuffer
    buf.delete(0, buf.length)
  }
  def interpret(s: String) = {
    clear(writer)
    var result: InterpreterResults.Result = InterpreterResults.Success
    var constructedLine: Option[String] = None
    val output = new java.lang.StringBuffer
    s.split("\n") foreach { line =>
      constructedLine = constructedLine match {
        case None => {
          if (line.trim.length > 0) Some(line)
          else None
        }
        case Some(cl) => Some(cl + line + "\n")
      }

      result = constructedLine match {
        case Some(cl) => {
          val res = interpreter.interpret(cl)
          res
        }
        case None => InterpreterResults.Incomplete
      }
      result match {
        case InterpreterResults.Error => ()
        case InterpreterResults.Incomplete => // no action necessary
        case InterpreterResults.Success => constructedLine = None
      }
      output.append("\n" + writer.toString)
      clear(writer)
    }
    if (output.toString.contains("error") || output.toString.contains("at ")) 
      removeEmptyLines(output.toString.trim)
    else 
      lastBlock(output.toString)
  }
  private def lastBlock(s: String) = {
	s.split("\n").foldLeft("") { (res, cur) => 
	  if (cur.trim.isEmpty)
	    ""
	  else if (!res.isEmpty) res + "\n" + cur.trim
	  else cur.trim
    }
  }
  private def removeEmptyLines(result: String) = result.split("\n").foldLeft("") { (res, cur) =>
    if (cur.isEmpty)
      res
    else
      res + "\n" + cur
  }
}
object ScalaInterpreter extends ScalaInterpreter