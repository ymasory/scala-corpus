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
import org.specs.specification._
import org.specs.util._
import org.specs._
import scala.xml._

class xmlRunnerSpec extends HtmlSpecificationWithJUnit with RunnerFixture { "The specification for the XML runner" is <t>

  A specification can be run by a XML runner object. The XML runner object is responsible for
  collecting the results of sub-specifications, systems under test and examples and organize
  them hierarchically as xml elements.

<p/>  1. File creation
<p/>  1.1 Simple file creation

<p/> Running an XML runner on a specification should create a file whose path is the name of the specification.
  For example, running the specification named "sp1" should create the path {
    "./org.specs.runner.sp1.xml".as(path) + " should be the name of the specification" in checkFilePath }

<p/>  1.2 Output directory

<p/>  It is possible to indicate the output directory of the runner, for example: {"specresults" as runnerOutputDir}
      In that case, {"the xml file should be created in the output directory with path: " +
                 "./specresults/org.specs.runner.sp1.xml".as(path) in checkOutputDirectory}

<p/> 2. XML content

<p/>  Running an XML runner on a specification should create an xml structure:

  
<ul>
  { <spec name="sp1" description="sp1" expectations="3" failures="1" errors="1"/>.as(xml) }
  <li>{"containing an element for the specification:" in checkXml }</li> 
    { xml().toString }<p/>
  
  { <sus description="the sus" expectations="3" failures="1" errors="1"/>.as(xml).as(xml) }
  <li>{ "containing an element for the system under test"  in checkXml }</li>
    { xml().toString }<p/>
  
  { <example description="have one ok example" expectations="0" failures="0" errors="0"></example>.as(xml).as(xml) }
  <li>{ "containing an element for the ok example test" in checkXml }</li>
   { xml().toString }<p/>
  
  { <example expectations="1" failures="1" description="have one ko example" errors="0">
          <failure location="xmlRunnerFixture.scala:66">'1' is not the same as '2'</failure>
    </example>.as(xml).as(xml) }
  <li>{ "containing an element for the ko example test containing the failure" in checkXml }</li>
    { xml().toString }<p/>
  
  { <example description="have an example with an error" expectations="1" failures="0" errors="1">
      <error location="xmlRunnerFixture.scala:67">error message</error>
    </example>.as(xml).as(xml) }
  <li>{ "containing an element for the ko example test containing the exception" in checkXml }</li>
    { xml().toString }<p/>

  { <example expectations="1" failures="0" description="have one sub-example" errors="0">
          <example expectations="1" failures="0" description="a sub-example" errors="0"></example>
    </example>.as(xml)}
  <li>{ "containing an element for the example containing a sub-example" in checkXml }</li>
    { xml().toString }<p/>
</ul>
<p/>
  If the XML runner is run on a composite specification{executeCompositeSpecRunner},
  it must then create a specification element containing the sub-specifications 
<ul>
  { <spec name="compositeSpec" description="compositeSpec" expectations="6" failures="2" errors="2"/> as xml }
  <li><ex>with one node for the first sub-specification</ex>{ checkXml }</li>
    { xml().toString }<p/>
    
  { <spec name="sp1" description="sp1" expectations="3" failures="1" errors="1"/> as xml }
  <li><ex>with another node for the other sub-specification</ex></li>{ checkXml }
  { xml().toString }<p/>
</ul >

<p/>  3. Console reporting

<p/> The XML runner object should {"output the results of the specification in the console" in checkConsole},
     as if it was a ConsoleRunner if it has been added the Console trait.

</t>
}
