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
import org.specs.literate.Textile

class htmlRunnerSpec extends htmlRunnerRules("Html Runner") with Textile { "Html Runner Specification" is <t>

A specification can be run and its output displayed as an Html page.
On this html page we should be able to see:

* statistics about the specification execution
* the list of all examples, sorted by sub-specifications and systems
* an overview list of all sub-specifications and systems with a status icon to allow a rapid access

h3. Specification Title

<ex>The title of the html page should be the title of the specification.</ex>{title}

h3. Specifications headers

<ex>There should be one header per specification</ex>{subSpecsHeader}

h3. System tables

<ex>There should be a table for each system under test.</ex>{oneTablePerSus}
<ex>The table must be preceded by the system name as a separate header.</ex>{systemName}

<ex>Close to the system name, there should be a small _up_ arrow !images/up.gif! to go to the top of the page</ex>{topArrow}
<ex>In each table, there should be a row per example</ex>{oneRowPerExample}

h3. Example rows

On each row, there should be:

* <ex>the description of the example</ex>{exampleDescription}
* <ex>a success image if the example succedeed</ex>{exampleSuccess}
* <ex>a failure image if the example failed</ex>{failedExampleImage}
* <ex>an error image if the example has an error</ex>{errorExampleImage}
* <ex>an info image if the example is skipped</ex>{skippedExampleImage}
* <ex>a failure message if any</ex>{failedExample}
* <ex>an exception message if any</ex>{errorExample}
* <ex>a skip message if any</ex>{skippedExample}

  <ex>The rows must alternate in style for better visibility</ex>{rowsAlternation}

h4. Subexamples

An example can also have sub-examples. In that case, <ex>the description of the example must be displayed as a title and sub-examples displayed in a table</ex>{subExamples}

h4. DataTables

<ex>DataTables failures should be displayed as an inner table in the message cell</ex>{dataTableFailure}

h4. Literate descriptions

<ex>Literate descriptions should be displayed above their table</ex>{literateDesc}

<ex>In a literate description the example description which succeeded must be highlighted in green</ex>{greenHighlight}

<ex>If the example contains a DataTableFailureException, then the table rows showing the results must be displayed</ex>

{""""
  "A calculator can add integers" inTable
    "a" | "b" | "c" |
     1  !  2  !  3  |
     2  !  2  !  5  |
     2  !  6  !  8  |> { (a:Int,b:Int,c:Int) => c must (==calc.add(a, b)) }

""" >@}

h3. Summary

<ex>A column with the list of systems should be available on the left to access a given system directly</ex>{susList}
<ex>Yet this column should only be displayed if there is more than one system</ex>{noSystemsListForOneOnly}
<ex>This column should be collapsible/expensible by clicking on an icon</ex>{collapsibleColumn}

h3. Output directory

h4. File name

The output of an HtmlRunner can be specified by specifiying an output directory.
In that case, <ex>the runner generates a file named after the specification name + .html in that directory.</ex>{outputFile}

h4. Stylesheets and images

<ex>The stylesheets for the report must be created in a directory named css, relative to the output directory.</ex>{cssDir}
<ex>The images for the report must be created in a directory named images, relative to the output directory.</ex>{imagesDir}

h4. Breadcrumbs

<ex>When the current specification has some parent specifications, they must be displayed a the top in a breadcrumb fashion, providing
links to access the parent specifications</ex>{breadcrumbs}

</t>
}