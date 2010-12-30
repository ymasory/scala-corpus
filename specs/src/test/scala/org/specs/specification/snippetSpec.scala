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
import org.specs._
import org.specs.runner._
import org.specs.util.Property

class snippetSpec extends SpecificationWithJUnit with Snippets {
  "The Snippets trait" should allow {
    val it = Property(Snippet())
    "a property to store a prelude current snippet" in {
      "import org.specs._" prelude it
      it.get.code must include("import")
    }
    "a property to store a body" in {
      "object s extends Specification" snip it
      it.get.code must include("Specification")
    }
    "a property to store a prelude and body" in {
      "import org.specs._" prelude it
      "object s extends Specification" snip it
      it.get.code must include("import") and include("Specification")
    }
    "to add code to a body" in {
      "object s extends Specification" snip it
      "{}" addTo it
      it.get.code must_== "object s extends Specification\n{}"
    }
    "to reset added code to a body by re-snipping" in {
      "object s extends Specification" snip it
      "{}" addTo it
      "println(1)" snip it
      it.get.code must not include("object") and include("println(1)")
    }
    "to reset added code to a body by re-snipping, keeping the prelude" in {
      "import org.specs._" prelude it
      "object s extends Specification" snip it
      "{}" addTo it
      "println(1)" snip it
      it.get.code must include("import")
    }
  }
  def allow = addToSusVerb("allow")
  "A snippet" should {
    "have a prelude method prepending some code to a snippet" in {
      Snippet(body = "body").prelude("prelude").code must include("prelude")
    }
    "have a body method appending some code to a snippet" in {
      Snippet(prelude = "prelude").body("body").code must include("body")
    }
    "cumulate preludes when using the prelude update method" in {
      val s = Snippet(prelude = "prelude1").prelude("prelude2")
      s.code must include("prelude1") and include("prelude2")
    }
    "cumulate bodies when using the body update method" in {
      val s = Snippet("prelude", "body").body("body2")
      s.code must include("body") and include("body2")
    }
    "have a ++ method adding 2 snippets together, appending preludes and bodies" in {
      val s1 = Snippet("prelude", "body")
      val s2 = Snippet("prelude2", "body2")
      (s1 ++ s2).code must include("prelude\nprelude2") and include("body\nbody2")
    }
    "add a newline only if necessary between the prelude and the body" in {
      Snippet("prelude\n", "body").code must_== "prelude\nbody"
      Snippet("prelude", "body").code must_== "prelude\nbody"
    }
  }
}
