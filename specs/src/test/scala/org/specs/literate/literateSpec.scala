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
import org.specs.runner._

class literateSpec extends LiterateSpecRules with Textile {
  override def htmlDir = "target"
  <t> 
h3. Description

A literate specification is a text embedded in xml tags.

h3. Code inside the text description

A literate specification can execute code by enclosing it in accolades: {"{ (1 + 1) }" >@}.
It is possible to silence the code being executed by using several functions, for example the @shh@ function:
  
   {"{ (1 + 1).shh }" >@}{ (1 + 1).shh }

or the {"@<|@"} operator at the end of any expression: {"{ 1 + 1 <| }" >@}{ 1 + 1 <| }

h3. Examples

h4. Named examples

<ex>An expectation can be included in a literate specification as part of an example</ex>, like this:
 {""" "1 must be 1" in { 1 must be(1) } """ >@} {exampleOk}

h4. Anonymous examples

<ex>The description from an example can also come from part of the description enclosed with an @ex@ xml tag</ex>, like that:

* {"@<ex>1 must be == to 1</ex> { 1 must be_==(1)  }@"}{taggedExample}

h4. Tags

<ex>It is possible to specify tags on the examples by using the @tag@ or @tags@ attributes.</ex>{taggedExamples}
Tag names must be a comma-separated list of values like that:

* {"@<ex tags=@"}@"group1, group2"@{"@/>@"}

h4. Not implemented

It is possible to mark an example as not implemented yet:

* {"@<ex> this example is not yet implemented </ex> { notImplemented }@"}

<ex>In that case, the example will be added but marked as skipped</ex>{notImplementedExample}

h3. Formatting

The format of the description is done by mixing-in the appropriate trait: Textile (default), Markdown, Html, Text. 

* <ex>The Text trait indicates text interpreted as simple text</ex>{isText}
* <ex>The Textile trait indicates text interpreted as Textile markup language</ex>{isTextile}
* <ex>The Markdown trait indicates text interpreted as Markdown markup language</ex>{isMarkdown}
* <ex>The Html trait indicates text interpreted as a html</ex>{isHtml}

h3. Properties

<ex>Part of the text can be stored as properties and reused later as expected values</ex>:
  
  { """After prompting for a name like {"Peter".a}, the system must greet the visitor with the person name: {"hello Peter".it}{ it must be_==(hello(a)) }""" >@ }

gives:
   {"<ex class=\"success\">"}After prompting for a name like _{"Peter".a}_, the system must greet the visitor with the person name: _{"hello Peter".it}_{ it() must be_==(hello(a)) }{"</ex>"}

</t> isSus
}
