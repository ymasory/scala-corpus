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
package org.specs.literate
import org.specs.Specification

/**
 * This trait supports writing literate specifications for acceptance testing.
 * 
 * The basic idea is to define Systems under specification as some text, enclosed in xml tags:
 * <code>
 * "my system" is <text>
 *   some text for my Specification
 * 
 * </text>
 * </code>
 * 
 * Then, it is possible to "tag" parts of this text with the <ex></ex> xml nodes, to denote an example description and make it immediately
 * followed by an expectation in Scala code: <code>
 * "my system" is <text>
 *   <ex>This is an ok example</ex> { 1 must be equalTo(1) }
 * 
 *   <ex>This is an ok example with several expectations which must be enclosed in an 'eg' function</ex> 
 *   { eg { 
 *       1 must be equalTo(1) 
 *       2 must be equalTo(2) 
 *     }
 *   }
 *   <ex>This is a example but missing its implementation yet</ex> { notImplemented }
 * 
 * </text>
 * </code>
 * 
 */
class LiterateSpecification extends Specification with LiterateBaseSpecification with LiterateSpecificationLinks 
      with LiterateDataTables with LiterateForms with LiterateProperties with LiterateShortcuts {
  setSequential()
  shareVariables()

  def this(n: String) = { this(); name = n; description = n; this }
}
