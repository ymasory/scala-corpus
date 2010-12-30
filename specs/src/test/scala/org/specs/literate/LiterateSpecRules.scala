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
import org.specs.Sugar._
import org.specs._
import org.specs.util._

class LiterateSpecRules extends HtmlSpecificationWithJUnit with AllProperties {

   object example1 extends LiterateSpecification with Text {
     <t>{"1 must be 1" in {1 must_== 1}}</t> isSus  }
   object example2 extends LiterateSpecification with Textile {
     <t>In this example <ex>*1 must be 1*</ex> { 1 must_== 1  } </t> isSus  }
   object example3 extends LiterateSpecification with literate.Html {
     <html><i><ex>this example is not yet implemented</ex></i> { notImplemented }</html> isSus  }
   object example5 extends LiterateSpecification with Markdown {
     <t><ex>_1 must be 1_</ex> { 1 must_== 1  }</t> isSus  }
   object example4 extends LiterateSpecification  {
     <t>
     <ex tags="included">this example is included</ex> { 1 must_== 1 }
     <ex>this example is not included</ex> { 1 must_== 0 }
     </t> isSus  }

   def exampleOk = checkSuccess(example1)
   def taggedExample = checkSuccess(example2)
   def notImplementedExample = checkSkipped(example3)
   def checkSuccess(s: Specification) = {
     s.systems.flatMap(_.examples).flatMap(_.failures).size aka "the number of failures" must_== 0
   }
   def checkSkipped(s: Specification) = {
     s.systems.flatMap(_.examples).flatMap(_.skipped).size aka "the number of skipped" must_== 1
   }
   def desc(s: Specification) = s.systems.head.literateDesc.toString aka "the formatted description"
   def isText = desc(example1) must include("1 must be 1")
   def isTextile = desc(example2) must include("<strong>1 must be 1</strong>")
   def isMarkdown = desc(example5) must include("<em>1 must be 1</em>")
   def isHtml = desc(example3) must include("<i><ex") and include ("</ex></i>")
   def taggedExamples = {
     example4.successes.size aka "the number of successes" must_== 1
   }
   
   import org.specs.util.AllProperties._

   def hello(name: String): String = "hello " + name
}