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
package org.specs.specification
import org.specs.util._
import org.specs.literate._

/**
 * This trait provides ways to create a piece of code, a "Snippet".
 * 
 * A Snippet can be created from a String and is stored in a Property[Snippet]
 * <code>
 * val it = Property(Snippet(""))
 * 
 * // update the "it" property with a Snippet object containing a specific prelude
 * "import org.specs._" prelude it 
 * 
 * // update the it property with a Snippet containing both the prelude and the mySpec declaration 
 * "object mySpec extends Specification" snip it
 * 
 * // update the it property with a Snippet containing both the prelude, the mySpec declaration
 * // and the list declaration 
 * "val l = List(mySpec)" addTo it
 * 
 * </code>
 */
trait Snippets extends ScalaInterpreter {
  
  /** 
   * transform a String to a SnippetAdder object to allow the creation of snippets
   * by using the prelude or snip method.
   */	
  implicit def asSnippet(s: String) = new SnippetAdder(s)

  /** 
   * add operations to a Snippet object to allow composition with other snippets.
   */	
  class SnippetAdder(code: String) {
    /** alias for addTo */	
	def add(s: Snippet): String = addTo(Property(s))
    /** 
     * add 2 snippets together 
     * @return the resulting formatted code
     */	
	def addTo(s: Snippet): String = addTo(Property(s))
    /** alias for addTo */	
	def add(prop: Property[Snippet]): String = addTo(prop)
    /** 
     * add to a snippet Property 
     * @return the resulting formatted code
     */	
	def addTo(prop: Property[Snippet]): String = {
	  prop.forceUpdate(prop.get ++ new Snippet().body(code))
	  formatCode(code)
	}
    /** 
     * prelude a snippet with a string 
     * @return the resulting formatted code
     */	
	def prelude(snippet: Snippet): String = prelude(Property(snippet))
    /** 
     * prelude a snippet with a string 
     * @return the resulting formatted code
     */	
	def prelude(prop: Property[Snippet]): String = updateSnippet(prop, (s: Snippet) => s.prelude(code))
    /** 
     * initialize the body of a snippet with a string 
     * @return the resulting formatted code
     */	
	def snip(snippet: Snippet): String = snip(Property(snippet))
    /** 
     * initialize the body of a snippet (stored in a Property) with a string 
     * @return the resulting formatted code
     */	
	def snip(prop: Property[Snippet]): String = updateSnippet(prop, (s: Snippet) => s.body(code))
	
	private def updateSnippet(prop: Property[Snippet], f: Snippet => Snippet) = {
	  prop.update(f) 
	  prop.map(s => formatCode(s.code)).getOrElse("")
	}
  }

  def execute(it: Property[Snippet]) = interpret(it().code)
  
  def formatCode(code: String): String = code

}
/**
 * This object can be used to import Snippets functionalities 
 */
object Snippets extends Snippets

/**
 * A Snippet is an object representing a piece of code with:
 * - an optional prelude
 * - an optional body
 *  
 * When 2 snippets are added together with the ++ method:
 * 
 * - the 2 preludes are appended
 * - the 2 bodies are appended
 */
case class Snippet(prelude: String = "", body: String = "") {

  /** 
   * add 2 snippets together, appending their preludes and their bodies
   * @return a new Snippet 
   */
  def ++(other: Snippet): Snippet = new Snippet(append(prelude, other.prelude), append(body, other.body))
  /**
   * prelude the code body with some other code
   * @param p the prelude code to append before the snippet body
   * @return a new Snippet
   */
  def prelude(p: String): Snippet = new Snippet(append(prelude, p), body)
  /**
   * update the body with something new
   * @param b the new body
   * @return a new Snippet
   */
  def body(b: String): Snippet = new Snippet(prelude, b) 
  /**
   * @return the snippet = prelude + body as a String
   */
  def code = append(prelude, body)
  
  /**
   * append 2 pieces of code, adding a newline between the 2 if necessary
   * @param a first piece of code
   * @param b second piece of code
   * @return the appended code a + b
   */
  private def append(a: String, b: String): String = {
    if (a.isEmpty)
      b
    else if (b.isEmpty)
      a
    else if (a.endsWith("\n"))
      a + b
    else 
      a + "\n" + b
  }
}