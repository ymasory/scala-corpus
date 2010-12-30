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
import org.specs.io._
import org.specs.util._
import org.specs.log._
import java.io.Writer
import org.specs.specification.BaseSpecification
import org.specs.util.ExtendedString._
import org.specs._

/**
 * This trait groups the functionalities of runners creating files from
 * specification executions.
 * 
 * It requires the definition of:<ul>
 * <li>a file name function</li>
 * <li>an output directory function</li>
 * <li>a specs holder having all the specifications to report</li>
 * <li>a specOutput function creating the output string to copy to the target file</li>
 * </ul>
 * 
 * Concrete implementations of that trait can be found in HtmlRunner and XmlRunner.
 */
trait File extends FileSystem with ConsoleLog with SpecsHolder with Console {
  /** @return the file name which should be created */
  def fileName(spec: BaseSpecification): String
  
  /** @return the output directory path */
  def outputDir: String
  
  /**
   * the default path is the output dir + file name  
   */
  def filePath(spec: BaseSpecification) = normalize(outputDir) + fileName(spec)

  /**
   * get the specification output from specOutput and write it to the target file.
   */
  override def report(specifications: Seq[Specification]): this.type = {
    super.report(specifications)
    mkdirs(outputDir)
    specifications foreach { spec => 
      write(filePath(spec)) { out: Writer =>
        out.write(specOutput(spec))
      }
    }
    this
  }

  /**
   * return the String representing the output of the specification execution.
   */
  def specOutput(spec: Specification): String
  /**
   * @returns a path with Unix like path separators and a final / separator 
   */
  def normalize(dir: String) = {
    var properDir = dir.replaceAll("\\\\", "/")
    if (!properDir.startsWith("/") && !properDir.startsWith("."))
      properDir = ("./" + properDir)
    if (!properDir.endsWith("/"))
      properDir += "/"
    properDir
  }
}

/** 
 * A FileReporter class is used to have a constructor setting the required variables of the File trait.
 */
abstract class FileReporter(outputDirPath: String, fName: BaseSpecification => String) extends File {
  override def fileName(s: BaseSpecification) = fName(s)
  override def outputDir = normalize(outputDirPath)
}

/** 
 * A FileSuite is like a FileReporter but supports being extended with the JUnit trait
 */
import _root_.org.junit.runner._
@RunWith(classOf[JUnitSuiteRunner])
abstract class FileSuite(outputDirPath: String, fName: BaseSpecification => String) extends File {
  override def fileName(s: BaseSpecification) = fName(s)
  override def outputDir = normalize(outputDirPath)
}
object NamingFunction {
  val default = (s: BaseSpecification) => s.getClass.getName.takeWhile(_ != '$').mkString
  val short = (s: BaseSpecification) => default(s).split("\\.").toList.last
}

