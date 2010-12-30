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
import org.specs.specification.BaseSpecification
import org.specs.Specification

class HtmlRunner(val specs: Seq[Specification], outputDirPath: String, fName: BaseSpecification => String) extends FileReporter(outputDirPath, fName) with Html {
  
  /** 
   * Alternative constructor with no specific specifications and default values.
   * This runner can report specifications using the report method.
   * This capacity is used in conjunction with the Runner class:<pre>
   * object runner extends Runner(spec1 :: spec2, HtmlRunner() :: XmlRunner())
   * </pre>
   */
  def this() = this(Nil, "./", HtmlNamingFunction.default)
  
  /** Alternative constructor with a default value for the output directory. */
  def this(specifications: Specification*) = this(specifications, "./", HtmlNamingFunction.default)
  
  /** Alternative constructor with one specification only. */
  def this(specification: Specification, outputDirPath: String) = this(List(specification), outputDirPath, HtmlNamingFunction.default)

  /** Alternative constructor with one specification only. */
  def this(spec: Specification, outputDirPath: String, fName: BaseSpecification => String) = this(List(spec), outputDirPath, fName)

  /** definition of the file name of a specification. */
  override def fileName(spec: BaseSpecification): String = fName(spec) 

  /** definition of the output directory of the report. */
  override def outputDir = normalize(outputDirPath)

}
/**
 * The HtmlSuite class is almost the same as the HtmlRunner class but can be extended with a JUnit trait.
 */
case class HtmlSuite(val specs: Seq[Specification], outputDirPath: String, fName: BaseSpecification => String) extends FileSuite(outputDirPath, fName) with Html {
  
  /** 
   * Alternative constructor with no specific specifications and default values.
   * This runner can report specifications using the report method.
   * This capacity is used in conjunction with the Runner class:<pre>
   * object runner extends Runner(spec1 :: spec2, HtmlRunner() :: XmlRunner())
   * </pre>
   */
  def this() = this(Nil, "./", HtmlNamingFunction.default)
  
  /** Alternative constructor with a default value for the output directory. */
  def this(specifications: Specification*) = this(specifications, "./", HtmlNamingFunction.default)
  
  /** Alternative constructor with one specification only. */
  def this(specification: Specification, outputDirPath: String) = this(List(specification), outputDirPath, HtmlNamingFunction.default)

  /** Alternative constructor with one specification only. */
  def this(spec: Specification, outputDirPath: String, fName: BaseSpecification => String) = this(List(spec), outputDirPath, fName)

  /** definition of the file name of a specification. */
  override def fileName(spec: BaseSpecification): String = fName(spec) 

  /** definition of the output directory of the report. */
  override def outputDir = normalize(outputDirPath)

}
