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
import org.specs.log.ConsoleLog
import org.specs.specification._
import scala.xml._

trait WikiFormatter extends LiterateDescriptionFormatter with ConsoleLog with Wiki {
  def format(desc: Elem, examples: Iterable[Example]): Node = {
    val parsed = parseToHtml(setStatus(desc.child.text, examples))
    try {
      XML.loadString("<html>" + parsed.replace("<?xml version='1.0' encoding='utf-8' ?>", "") + "</html>").child(0)
    } catch {
      case e => Text(e.getMessage + "\\n" + parsed)
    }
  }
  def setStatus(desc: String, examples: Iterable[Example]) = {
    var result = desc
    examples foreach { example =>
      def toReplace = escapeHtml("<ex class=\"" + example.statusClass + "\" " + onmouse(example) + ">" +
                      format(example.description) + "</ex>")
      if (result.contains(example.exampleDescription.format))
        result = result.replace(example.exampleDescription.format, toReplace)
      else
        result = result.replace(example.exampleDescription.toString, toReplace)
    }
    result
  }
  def escapeHtml(s: String) = s
  def format(desc: String): String = {
    val parsed = parseToHtml(desc)
    if (parsed contains "<p>") {
      val p1 = parsed.substring(parsed.indexOf("<p>") + 3, parsed.size)
      p1.substring(0, p1.indexOf("</p>"))
    } else
      parsed
  }
  protected def parseToHtml(s: String) = s

  override def format(desc: Elem): Node = format(desc, Nil)
  
  override def formatDesc(ex: Example): Node = {
    XML.loadString("<t>" + format(ex.exampleDescription.toString) + "</t>")
  }
  private def onmouse(example: Example) = {
    var function = if (example.hasIssues) "showExampleMessage('" + example.statusClass.capitalize + "','" else "showExampleDesc('"
    "onmouseover=\"" + function + System.identityHashCode(example) +"',event)\" " +
    "onmouseout=\"hideToolTip();\""
  }
}
