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
import scala.xml._
import org.specs.specification._
import org.specs.io._
import org.specs.util._
import org.specs.util.ExtendedThrowable._
import org.specs.xml.NodeFunctions._
import org.specs.execute._
import org.specs._

/**
 * The Html trait outputs the results of a specification execution as an html
 * file in a given output directory.
 *
 * The default file name for the report is "specs-report.html" and that report
 * contains all examples description with their execution status: error, failure, success, skipped.
 */
trait Html extends File {
  /** definition of the file name of a specification. */
  override def fileName(spec: BaseSpecification): String = HtmlNamingFunction.default(spec)

  /** definition of the output directory of the report. */
  override def outputDir = normalize(htmlDir)

  /** default directory name. */
  def htmlDir = "target"

  /** report the specification held by this runner. */
  override def report(specifications: Seq[Specification]) = {
    // reuse the inherited method using the specOutput method
    super.report(specifications)
    // provide the additional resources for the html files
    copySpecResourcesDir("images", outputDir)
    copySpecResourcesDir("css", outputDir)

    debug("Html - reporting to " + outputDir + ": " + specs.map(_.description).mkString(", "))
    this
  }
  
  /** define the html content for this specification execution. */
  def specOutput(spec: Specification): String = {
    // it is necessary to replace the br blocks because:
    // <br></br>: gets interpreted as a double line break in html
    // </br>: gets interpreted as a single line break in html
    asHtml(spec).toString.replace("<br></br>", "</br>")
  }

  /**
   * Create the html content for this specification execution.
   *
   * The html page is composed of a left column with a table summarizing the status for all systems
   * and a right (larger) column with a table containing all examples.
   */
  def asHtml(spec: Specification): Elem = <html>
    {head(spec)}
    <body>
      {breadcrumbs(spec)}
      <div id="toolTip"/>
      {anchorName("top")}
      {summaryTable(spec)}
      <div id="bodyColumn">
        {specificationTable(spec, spec.planOnly())}
      </div>
    </body>
  </html>

  /**
   * head declaration for the specification.
   *
   * The title of the document is the specification name.
   */
  def head(specification: Specification) = <head>
      <title>{specification.name}</title>
        <style type="text/css" media="all">
          @import url('./css/maven-base.css');
          @import url('./css/maven-theme.css');
          @import url('./css/site.css');
        </style>
        <link href="./css/prettify.css" type="text/css" rel="stylesheet" />
        <script type="text/javascript" src="./css/prettify.js"></script>
        <link rel="stylesheet" href="./css/print.css" type="text/css" media="print" />
        <link href="./css/tooltip.css" rel="stylesheet" type="text/css" />
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <script type="text/javascript" src="./css/tooltip.js"/>
        {javaScript(specification)}
        <script language="javascript">window.onload={"init;"}</script>
        <!-- the tabber.js file must be loaded after the onload function has been set, in order to run the
             tabber code, then the init code -->
        <script type="text/javascript" src="./css/tabber.js"></script> 
        <link rel="stylesheet" href="./css/tabber.css" type="text/css" media="screen"/> 
    </head>

  /** create breadcrumbs links for a specification, starting with the oldest parent */
  def breadcrumbs(spec: Specification) = {
    if (!spec.parentSpecifications.isEmpty) {
      <div id="breadcrumbs">{reduce[BaseSpecification](spec.parentSpecifications.reverse, 
                             (s: BaseSpecification) => Text("> ") ++ specificationLink(s) )}</div>
    } else NodeSeq.Empty
  }
  /** @return the path to a specification report file */
  def specificationLink(spec: BaseSpecification) = {
    val filePath = spec match {
      case s: BaseSpecification with Html => s.fileName(spec)
      case _ => HtmlNamingFunction.default(spec)
    }
    <a href={filePath}>{spec.name} </a>
  }
  /**
   * returns a table with the name of all systems, with their status,
   * possibly shortened if the system's description is too long.
   */
  def summaryTable(specification: Specification) = {
    /** returns the title of the specification spanning 2 columns for the summary table. */
    def specNavHeader = {
      <tr>
        <td><img src="images/expanded.gif" onclick="toggleNavBar(this)"/></td>
        <td class="navTitle">{specification.name}</td>
      </tr>
    }
    <div id="leftColumn">
     { if (nonTrivialSpec(specification)) {
        <table>
          { specNavHeader }
          { summarySpec(specification) }
        </table> }
      else
        NodeSeq.Empty }
    </div>
  }

  def summarySpec(spec: Specification): NodeSeq =
    reduce[Sus](spec.systems, summarySus(_, spec)) ++
    reduce[Specification](spec.subSpecifications, summarySpec(_))

 /** creates a summary row for a sus. */
  def summarySus(sus: Sus, spec: Specification): NodeSeq = <tr>
    <td>{statusIcon(sus, spec.planOnly())}</td>
    <td>{anchorRef(susName(sus, spec))}</td>
  </tr>

  /**
   * creates an anchor reference for a given name,
   * possibly shortening it for the left column display, but leaving the full name
   * as a tooltip.
   */
  def anchorRef(name: String) = {
    <a href={"#" + sanitize(name)} title={name}>{shorten(name)}</a>
  }
  /** creates an anchor name, sanitizing the name. */
  def anchorName(name: String) = <a name={sanitize(name)}/>

  /** sanitize a string so that it can be used as a href */
  def sanitize(s: String) = java.net.URLEncoder.encode(s, "UTF-8")

  /** shorten a string to 30 characters maximum. */
  def shorten(s: String) = if (s.size <= 27) s else (s.take(27) + "...")

  /** create tables for all the subspecifications. */
  def subspecsTables(subSpecs: List[Specification], planOnly: Boolean): NodeSeq = reduce[Specification](subSpecs, specificationTable(_, planOnly))

  /** create a table for one specification. */
  def specificationTable(spec: Specification, planOnly: Boolean) = {
    spec.linkedSpecifications.foreach(_.reportSpecs)
    <h2>{spec.description}</h2> ++ subspecsTables(spec.unlinkedSpecifications.toList, planOnly) ++ susTables(spec, planOnly)
  }
  /** create tables for systems. */
  def susTables(spec: Specification, planOnly: Boolean): NodeSeq = reduce[Sus](spec.systems, susTable(_, spec, planOnly))

  /** create a table for a system. */
  def susTable(sus: Sus, spec: Specification, planOnly: Boolean): NodeSeq = {
    anchorName(susName(sus, spec)) ++
    susHeader(sus) ++
    sus.literateDesc ++
    examplesTable(sus, planOnly)
  }

  /** return the sus header if it is not empty, otherwise return the spec name. */
  def susName(sus: Sus, spec: Specification) = if (sus.header.trim.isEmpty) spec.name else sus.header

  /** return the sus header if not empty or an Empty Node. */
  def susHeader(sus: Sus) = {
    if (!sus.header.trim.isEmpty)
      <h3>{sus.header}{upArrow}</h3>.toSeq
    else
      NodeSeq.Empty
  }

  def examplesTable(sus: Sus, planOnly: Boolean): NodeSeq = {
    sus.literateDescription match {
      case None => {
        <table class="bodyTable">
           {exampleRows(sus.examples, sus.isFullSuccess, planOnly)}
         </table>}
      case Some(_) if !sus.examples.isEmpty => {
        <h3><img src="images/collapsed.gif" onclick={"toggleImage(this); showHideTable('sus:" + System.identityHashCode(sus) + "')"}/>Examples summary</h3>
        <div id={"sus:" + System.identityHashCode(sus)} style="display:none">
          <table class="bodyTable">
             {exampleRows(sus.examples, sus.isFullSuccess, planOnly)}
          </table>
        </div>
      }
      case _ => NodeSeq.Empty
    }
  }
  /** create an up arrow with an anchor ref to the top. */
  def upArrow = <a href="#top">   <img src="images/up.gif"/></a>

  /** create rows for each example, alternating style. */
  def exampleRows(examples: Iterable[Example], fullSuccess: =>Boolean, planOnly: Boolean): NodeSeq = examples.toList.foldLeft((NodeSeq.Empty.toSeq, true)) { (result, ex) =>
    val (node, alternation) = result
    (node ++ example(ex, alternation, fullSuccess, planOnly), !alternation)
  }._1

  /**
   * create a row for an example and its subexamples.
   *
   * If the example has subexamples, a small header is created.
   */
  def example(example: Example, alternation: Boolean, fullSuccess: =>Boolean, planOnly: Boolean) = {
    example.examples.toList match {
      case Nil => exampleRow(example, alternation, fullSuccess, planOnly)
      case subexamples => <h4>{example.exampleDescription.toXhtml.text}</h4> ++ exampleRows(subexamples, fullSuccess, planOnly)
    }
  }
  /**
   * create a row for an example with its status, description and message.
   */
  def exampleRow(example: Example, alternation: Boolean, fullSuccess: =>Boolean, planOnly: Boolean) = {
    <tr class={if (alternation) "b" else "a"}>
      <td id={"rowdesc:" + System.identityHashCode(example)}>{statusIcon(example, planOnly)} {example.exampleDescription.toXhtml}</td>{message(example, fullSuccess, planOnly)}
    </tr>
  }

  /**
   * status icon for anything having results (errors, failures, skipped).
   */
  def statusIcon(result: HasResults): NodeSeq = statusIcon(result, false)
  def statusIcon(result: HasResults, planOnly: Boolean): NodeSeq = {
    <img src={image(result, planOnly)} id={"rowicon:" + System.identityHashCode(result)}/>
  }
  def image(result: HasResults, planOnly: Boolean) = {
    if (planOnly)
      "images/icon_success_sml.gif"
    else
      "images/icon_" + result.statusClass + "_sml.gif"
  }

  /** Message for an example. */
  def message(example: Example, fullSuccess: =>Boolean): NodeSeq = message(example, fullSuccess, false)
  def message(example: Example, fullSuccess: =>Boolean, planOnly: Boolean): NodeSeq = {
    def msg = {
      if (!planOnly && !example.failures.isEmpty)
          reduce[FailureException](example.failures, failure(_))
        else if (!planOnly && !example.errors.isEmpty)
          reduce[Throwable](example.errors, e => exceptionText(e))
        else if (!planOnly && !example.skipped.isEmpty)
          reduce[SkippedException](example.skipped, s => exceptionText(s))
        else
          ""
    }
    if (planOnly || fullSuccess)
      NodeSeq.Empty
    else
      <td id={"rowmess:" + System.identityHashCode(example)}>{msg}</td>
  }

  /**
   * the failure message for an example is displayed differently depending on its nature.
   * Failures for DataTables will be reported in a nested table.
   */
  def failure(f: FailureException): NodeSeq = {
    f match {
      case e: DataTableFailureException => e.table.toXhtml
      case regular => exceptionText(regular)
    }
  }
  def stackTrace(e: Throwable) = if (!e.isInstanceOf[FailureException]) e.stackToString("\r", "\r", "") else ""
  def exceptionText(e: Throwable) = <a title={e.fullLocation + stackTrace(e)}>{if (e.getMessage != null) new Text(e.getMessage) else new Text("null")}</a>

  def initFunction(specification: Specification) = {
    """function init() { """ +
     "prettyPrint()" + 
      (if (nonTrivialSpec(specification)) "" else ";noNavBar()") +
    "}"
  }
  def nonTrivialSpec(specification: Specification) = {
    (specification.systems ++ specification.unlinkedSpecifications).size > 1
  }
  def javaScript(specification: Specification) = <script language="javascript"> { 
    initFunction(specification) +
    """
    // found on : http://www.tek-tips.com/faqs.cfm?fid=6620
    String.prototype.endsWith = function(str) { return (this.match(str+'$') == str) }

    function changeWidth(id,width) {
      document.getElementById(id).style.width = width;
    }
    function changeMarginLeft(id, margin) {
      document.getElementById(id).style.marginLeft = margin;
    }
    function noNavBar() {
      changeWidth('leftColumn','0px');
      document.getElementById('leftColumn').style.visibility = 'hidden'; 
      document.getElementById('leftColumn').style.display = 'none'; 
      changeMarginLeft('bodyColumn', '35px')
   }
   function toggleNavBar(image) {
      toggleImage(image)
      if (image.src.endsWith('images/expanded.gif')) {
        changeWidth('leftColumn','20px');
        changeMarginLeft('bodyColumn', '35px')
      }
    else {
        changeWidth('leftColumn','250px');
        changeMarginLeft('bodyColumn', '277px')
      }
   }
   function toggleImage(image) {
      if (image.src.endsWith('images/expanded.gif')) {
        image.src = 'images/collapsed.gif';
      }
    else {
        image.src = 'images/expanded.gif';
      }
   }
    function showHideTable(tableId) {
      table = document.getElementById(tableId)
      table.style.display = (table.style.display == 'none')? 'block' : 'none';
    }
    function showExampleMessage(status, exId, event) {
    exampleMessage = document.getElementById('rowmess:' + exId)
    exampleIcon = document.getElementById('rowicon:' + exId)
    showToolTipWithIcon(status, exampleIcon.src, exampleMessage.innerHTML, event)
  }
    function showExampleDesc(exId, event) {
        exampleDesc = document.getElementById('rowdesc:' + exId)
        showToolTip('Description', exampleDesc.innerHTML, event)
    }

"""}
    </script>
}
object HtmlNamingFunction {
  val default = { (s: BaseSpecification) => NamingFunction.default(s) + ".html" }
  val short = { (s: BaseSpecification) => NamingFunction.short(s) + ".html" }
}
