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
import org.specs.util.Properties
/**
 * This trait provides functions which can be used to ease the use of wiki markup
 */
trait Wiki extends Properties with Links {
  implicit def toWikiString(a: Any) = new WikiString(a.toString)
  class WikiString(s: String) {
    def code = wikiCode(s)
    def >@ = wikiCode(s)
    def pre = wikiPre(s)
  }
  /**
   * This function can be used to format code in a wiki description.
   * Using this function avoid issues like quotes insides brackets ['something']
   * being displayed as question marks.
   */
  def wikiPre(stringToFormat: String) = <pre>{stringToFormat}</pre>
  def wikiCode(stringToFormat: String) = stringToFormat.replace("\r\n", "\n").
                                                        replace("\n\r", "\n").
          split("\n").map(htmlize(_)).mkString("==<code class=\"prettyprint\">", "</code>==<br/>==<code class=\"prettyprint\">", "</code>==")

  protected def htmlize(s: String) = s.replace("<", "&lt;").replace(">", "&gt;")
  /**
   * Alias for wikiCode
   */
  def >@(stringToFormat: String) = wikiCode(stringToFormat)

  def linkTo(susName: String) = "link to " + susName + " not implemented yet"
  override def pathLink(desc: String, path: String) = {
    "\"" + desc + "\":file:///" + path
  }
  override def relativeLink(desc: String, path: String) = {
    "\"" + desc + "\":" + path
  }
  def escapeMarkup = (s: String) => s
}
trait TextileWiki extends Wiki
