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

import org.specs.io.FileSystem
import java.util.regex._
import scala.collection.mutable.Queue
import org.specs.util.Classes
import org.specs.Specification
import org.specs.util.LazyParameter
/**
 * Companion SpecsFinder object to create a SpecsFinder returning an aggregate Specification of
 * all the found specifications.
 */
 object SpecsFinder {
  def apply(path: String, pattern: String) = new SpecsFinder(path, pattern, true)
}

/**
 * The SpecsFinder class can be used to hold the specifications found on a given path, with a given pattern.
 * Those specifications will either be combined into a big one or kept separate as a Sequence.
 */
case class SpecsFinder(path: String, pattern: String, asOneSpecification: Boolean) extends SpecificationsFinder with SpecsFilter {

  lazy val specs: Seq[Specification] = collectSpecs(asOneSpecification)

  protected def collectSpecs(asOneSpecification: Boolean): Seq[Specification] = {
    val collected = specificationNames(path, pattern).toStream.flatMap(createSpecification(_)) 
    if (asOneSpecification) {
	    object totalSpecification extends Specification {
	      declare(new java.io.File(path).getAbsolutePath).isSpecifiedBy(collected.map(s => new LazyParameter(() => s)).toSeq:_*)
	    }
	    List(totalSpecification)
    }
    else
      collected.toSeq
  }
}
/**
 * This trait browses files on a given path and returns what can be matching specification class names.
 */
trait SpecificationsFinder extends FileSystem with Classes {

   /**
    * @param path a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
    * @param pattern a regular expression which is supposed to match an object name extending a Specification
    * @return specification names by scanning files and trying to find specifications declarations
    */
   def specificationNames(path: String, pattern: String) : List[String] = {
     var result = new Queue[String]
     filePaths(path).foreach { collectSpecifications(result, _, pattern) }
     result.toList
   }

  /**
   * adds possible specification class names found in the file <code>filePath</code><br>
   * The specification pattern is: "\\s*object\\s*(" + pattern + ")\\s*extends\\s*.*Spec.*\\s*\\{"
   * This may be later extended to support other arbitrary patterns
   *
   * @param path a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   */
  def collectSpecifications(result: Queue[String], filePath: String, pattern: String): Unit = {
    if (!filePath.endsWith(".scala")) return
    def addClassNameFor(specType: String, suffix: String) = {
      val specPattern = "\\s*"+specType+"\\s*(" + pattern + ")\\s*extends\\s*.*"
      val m = Pattern.compile(specPattern).matcher(readFile(filePath))
      while (m.find) {
        result += ((packageName(filePath).map(_ + ".").getOrElse("") + m.group(1).trim) + suffix)
      }
    }
    addClassNameFor("object", "$")
    addClassNameFor("class", "")
  }

  /** @return the package declaration at the beginning of a file */
  def packageName(path: String) = {
    val pattern = "\\s*package\\s*(.+)\\s*"
    val m = Pattern.compile(pattern).matcher(readFile(path))
    if (!m.find)
      None
    else
      Some(m.group(1).replace(";", "").trim)
  }
  /**
   * @return a <code>Specification</code> object from a className if that class is a <code>Specification</code> class.<br>
   * Tries to load the class name and cast it to a specification
   * @return None in case of an exception.
   */
  def createSpecification(className: String): Option[Specification] = tryToCreateObject[Specification](className)
  /**
   * @return a <code>Specification</code> object from a className if that class is a <code>Specification</code> class.<br>
   * Tries to load the class name and cast it to a specification
   * @return None in case of an exception.
   */
  def createSpecification(className: String, printMessage: Boolean, printStackTrace: Boolean): Option[Specification] = createObject[Specification](className, printMessage, printStackTrace)
}
