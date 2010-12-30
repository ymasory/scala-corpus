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
import scala.xml._

trait LiterateBaseSpecification extends BaseSpecification with ExpectableFactory with WikiFormatter { outer =>
  implicit def toSus(e: => Elem): ToLiterateSus = new ToLiterateSus(e) 
  class ToLiterateSus(e: => Elem) {
    def isSus = toLiterateSusWithDesc("") ->> e
  }
  implicit def toLiterateSusWithDesc(string: String) = new LiterateSus(specify(string))
  implicit def toLiterateSus(sus: Sus) = new LiterateSus(sus)

  /** This class acts as an extension of a Sus to provide a literate description of a sus as an xml specification */
  class LiterateSus(sus: Sus) {
    def ->>(e: => Elem)= {
      sus.verb = ""
      format(e)
    }
    /** specifies the system with a literate description and embedded expectations */
    def is(e: => Elem)= {
      sus.verb = "specifies"
      format(e)
    }
    /** associates every <ex> tag to an anonymous example */
    private def format(e: => Elem) = {
      try {      
        var content: Elem = <nothing/> 
        sus.specifyExample {
          val evaluated = e
          content = evaluated
        }
        val anonymous = sus.examples.filter(_.description.matches("example \\d+"))
        val exNodes = content.\\("ex")
        exNodes.theSeq.toList.zip(anonymous.toList).foreach { pair =>
          val (node, example) = pair
          example.exampleDescription = makeExampleDescription(node)
          List("tag", "tags") foreach { tagName => addTag(node, example, tagName) }
        }
        sus.literateDescription = Some(LiterateDescription(outer.format(content, sus.examples)))
      } catch {
        case t => forExample("The system could not be evaluated").addError(t)
      }
    }
    private def addTag(node: Node, example: Example, tagName: String) = {
      node.attribute(tagName) match {
        case None => ()
        case Some(a) => a.toString.split(",").foreach(t => example.addTag(t.trim))
      }
    }
  }
}
