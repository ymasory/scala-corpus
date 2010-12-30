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
import org.specs.specification._
import org.specs._
import scala.xml._

/**
 * This trait allows to add links to other specifications inside a literate specification.
 * The link will be displayed as a Html link
 * 
 * The "parent/child" relationship is kept in that trait to allow the Html runner
 * to be reported when reporting the parent.
 */
trait LiterateSpecificationLinks extends LinkedSpecification with Links { this: Specification => 
  def linkTo(subSpec: Specification with runner.Html): String = linkTo(subSpec.description, subSpec)
  def linkTo(desc: String, subSpec: Specification with runner.Html): String = {
    super.linkTo(subSpec)
    // execute the subSpec
    subSpec.failures
    relativeLink(desc, subSpec.fileName(subSpec))
  }
  def linkTo(title: String, content: NodeSeq): String = { 
    linkTo(title, new HtmlSpecification(title) with Markdown { title is <m>{content.toString}</m> }) 
  }
}
