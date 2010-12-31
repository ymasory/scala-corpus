/*
 * Copyright 2007-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb {
package http {

import S._
import _root_.net.liftweb.common._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.http.js._
import _root_.net.liftweb.http.js.AjaxInfo
import JE._
import JsCmds._
import _root_.scala.xml._

/**
 * The SHtml object defines a suite of XHTML element generator methods
 * to simplify the creation of markup, particularly with forms and AJAX.
 */
object SHtml {

  /**
   * Convert a T to a String for display in Select, MultiSelect,
   * etc.
   */
  trait PairStringPromoter[T] extends Function1[T, String]

  /**
   * A companion object that does implicit conversions
   */
  object PairStringPromoter {
    implicit val strPromot: PairStringPromoter[String] =
      new PairStringPromoter[String]{ def apply(in: String): String = in}

    implicit val intPromot: PairStringPromoter[Int] =
      new PairStringPromoter[Int]{ def apply(in: Int): String = in.toString}

    implicit def funcPromote[T](f: T => String): PairStringPromoter[T] =
      new PairStringPromoter[T]{def apply(in: T): String = f(in)}
  }

  /**
   * An attribute that can be applied to an element.  Typically,
   * this will be a key-value pair, but there is a class of HTML5
   * attributes that should be similated in JavaScript
   */
  trait ElemAttr extends Function1[Elem, Elem] {
    /**
     * Apply the attribute to the element
     */
    def apply(in: Elem): Elem
  }

  /**
   * The companion object that has some very helpful conversion
   */
  object ElemAttr {
    implicit def pairToBasic(in: (String, String)): ElemAttr = 
      new BasicElemAttr(in._1, in._2)

    implicit def funcToElemAttr(f: Elem => Elem): ElemAttr = 
      new ElemAttr{def apply(in: Elem): Elem = f(in)}

    implicit def strSeqToElemAttr(in: Seq[(String, String)]):
    Seq[ElemAttr] = in.map(a => a: ElemAttr)
  }

  private class ApplicableElem(in: Elem) {
    def %(attr: ElemAttr): Elem = attr.apply(in)
  }

  private implicit def elemToApplicable(e: Elem): ApplicableElem =
    new ApplicableElem(e)

  /**
   * Any old attribute
   */
  final case class BasicElemAttr(name: String, value: String) extends ElemAttr {
    /**
     * Apply the attribute to the element
     */
    def apply(in: Elem): Elem = in % (name -> value)
  }

  /**
   * Invokes the Ajax request
   * @param in the JsExp that returns the request data
   */
  def makeAjaxCall(in: JsExp): JsExp = new JsExp {
    def toJsCmd = "liftAjax.lift_ajaxHandler(" + in.toJsCmd + ", null, null, null)"
  }

  /**
   * Invokes the Ajax request
   * @param in the JsExp that returns the request data
   * @param context defines the response callback functions and the response type (JavaScript or JSON)
   */
  def makeAjaxCall(in: JsExp, context: AjaxContext): JsExp = new JsExp {
    def toJsCmd = "liftAjax.lift_ajaxHandler(" + in.toJsCmd + ", " + (context.success openOr "null") +
            ", " + (context.failure openOr "null") +
            ", " + context.responseType.toString.encJs +
            ")"
  }

  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * 
   * @param jsCalcValue the JavaScript that will be executed on the client to calculate the value to be sent to the server
   * @param func the function to call when the data is sent
   *
   * @return the function ID and JavaScript that makes the call
   */
  def ajaxCall(jsCalcValue: JsExp, func: String => JsCmd): (String, JsExp) = ajaxCall_*(jsCalcValue, SFuncHolder(func))

  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * 
   * @param jsCalcValue the JavaScript that will be executed on the client to calculate the value to be sent to the server
   * @param jsContext the context instance that defines JavaScript to be executed on call success or failure
   * @param func the function to call when the data is sent
   *
   * @return the function ID and JavaScript that makes the call
   */
  def ajaxCall(jsCalcValue: JsExp, jsContext: JsContext, func: String => JsCmd): (String, JsExp) =
    ajaxCall_*(jsCalcValue, jsContext, SFuncHolder(func))

  /**
   * Build a JavaScript function that will perform a JSON call based on a value calculated in JavaScript
   * 
   * @param jsCalcValue the JavaScript to calculate the value to be sent to the server
   * @param jsContext the context instance that defines JavaScript to be executed on call success or failure
   * @param func the function to call when the data is sent
   *
   * @return the function ID and JavaScript that makes the call
   */
  def jsonCall(jsCalcValue: JsExp, func: Any => JsCmd): (String, JsExp) =
    jsonCall_*(jsCalcValue, SFuncHolder(s => JSONParser.parse(s).map(func) openOr Noop))

  /**
   * Build a JavaScript function that will perform a JSON call based on a value calculated in JavaScript
   * 
   * @param jsCalcValue the JavaScript to calculate the value to be sent to the server
   * @param jsContext the context instance that defines JavaScript to be executed on call success or failure
   * @param func the function to call when the data is sent
   *
   * @return the function ID and JavaScript that makes the call
   */
  def jsonCall(jsCalcValue: JsExp, jsContext: JsContext, func: Any => JsCmd): (String, JsExp) =
    jsonCall_*(jsCalcValue, jsContext, SFuncHolder(s => JSONParser.parse(s).map(func) openOr Noop))


  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param func -- the function to call when the data is sent
   *
   * @return the function ID and JavaScript that makes the call
   */
  private def jsonCall_*(jsCalcValue: JsExp, func: AFuncHolder): (String, JsExp) =
    fmapFunc(contextFuncBuilder(func))(name =>
            (name, makeAjaxCall(JsRaw("'" + name + "=' + encodeURIComponent(JSON.stringify(" + jsCalcValue.toJsCmd + "))"))))

  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param ajaxContext -- the context defining the javascript callback functions and the response type
   * @param func -- the function to call when the data is sent
   *
   * @return the function ID and JavaScript that makes the call
   */
  private def jsonCall_*(jsCalcValue: JsExp,
                         ajaxContext: AjaxContext,
                         func: AFuncHolder): (String, JsExp) =
    fmapFunc(contextFuncBuilder(func))(name =>
            (name, makeAjaxCall(JsRaw("'" + name + "=' + encodeURIComponent(JSON.stringify(" + jsCalcValue.toJsCmd + "))"), ajaxContext)))

  def fajaxCall[T](jsCalcValue: JsExp, func: String => JsCmd)(f: (String, JsExp) => T): T = {
    val (name, js) = ajaxCall(jsCalcValue, func)
    f(name, js)
  }

  def jsonCall(jsCalcValue: JsExp,
               jsonContext: JsonContext,
               func: String => JsObj): (String, JsExp) = ajaxCall_*(jsCalcValue, jsonContext, SFuncHolder(func))

  def fjsonCall[T](jsCalcValue: JsExp, jsonContext: JsonContext, func: String => JsObj)(f: (String, JsExp) => T): T = {
    val (name, js) = jsonCall(jsCalcValue, jsonContext, func)
    f(name, js)
  }

  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param func -- the function to call when the data is sent
   *
   * @return the JavaScript that makes the call
   */
  private def ajaxCall_*(jsCalcValue: JsExp, func: AFuncHolder): (String, JsExp) =
    fmapFunc(contextFuncBuilder(func))(name =>
            (name, makeAjaxCall(JsRaw("'" + name + "=' + encodeURIComponent(" + jsCalcValue.toJsCmd + ")"))))

  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param ajaxContext -- the context defining the javascript callback functions and the response type
   * @param func -- the function to call when the data is sent
   *
   * @return the JavaScript that makes the call
   */
  private def ajaxCall_*(jsCalcValue: JsExp,
                         ajaxContext: AjaxContext,
                         func: AFuncHolder): (String, JsExp) =
    fmapFunc(contextFuncBuilder(func))(name =>
            (name, makeAjaxCall(JsRaw("'" + name + "=' + encodeURIComponent(" + jsCalcValue.toJsCmd + ")"), ajaxContext)))


  private def deferCall(data: JsExp, jsFunc: Call): Call =
    Call(jsFunc.function, (jsFunc.params ++ List(AnonFunc(makeAjaxCall(data)))): _*)

  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   * @param attrs -- the list of node attributes
   *
   * @return a button to put on your page
   */
  def ajaxButton(text: NodeSeq, func: () => JsCmd, attrs: ElemAttr*): Elem = {
    attrs.foldLeft(fmapFunc(contextFuncBuilder(func))(name =>
            <button onclick={makeAjaxCall(Str(name + "=true")).toJsCmd +
                    "; return false;"}>{text}</button>))((e, f) => f(e))
  }

  /**
   * Create an Ajax buttun that when it's pressed it submits an Ajax request and expects back a JSON
   * construct which will be passed to the <i>success</i> function
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   * @param ajaxContext -- defines the callback functions and the JSON response type
   * @param attrs -- the list of node attributes
   *
   * @return a button to put on your page
   *
   */
  def jsonButton(text: NodeSeq, func: () => JsObj, ajaxContext: JsonContext, attrs: ElemAttr*): Elem = {
    attrs.foldLeft(fmapFunc(contextFuncBuilder(func))(name =>
            <button onclick={makeAjaxCall(Str(name + "=true"), ajaxContext).toJsCmd +
                    "; return false;"}>{text}</button>))((e, f) => f(e))
  }

  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   * @param attrs -- the list of node attributes
   *
   * @return a button to put on your page
   */
  def ajaxButton(text: NodeSeq, jsExp: JsExp, func: String => JsCmd, attrs: ElemAttr*): Elem = {
    attrs.foldLeft(fmapFunc(contextFuncBuilder(SFuncHolder(func)))(name =>
            <button onclick={makeAjaxCall(JsRaw(name.encJs + "+'='+encodeURIComponent(" + jsExp.toJsCmd + ")")).toJsCmd +
                    "; return false;"}>{text}</button>))((e, f) => f(e))
  }

  /**
   * Create an Ajax buttun that when it's pressed it submits an Ajax request and expects back a JSON
   * construct which will be passed to the <i>success</i> function
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   * @param ajaxContext -- defines the callback functions and the JSON response type
   * @param attrs -- the list of node attributes
   *
   * @return a button to put on your page
   *
   */
  def jsonButton(text: NodeSeq, jsExp: JsExp, func: Any => JsObj, ajaxContext: JsonContext, attrs: ElemAttr*): Elem = {
    attrs.foldLeft(jsonFmapFunc(func)(name =>
            <button onclick={makeAjaxCall(JsRaw(name.encJs + "+'='+ encodeURIComponent(JSON.stringify(" + jsExp.toJsCmd + "))"), ajaxContext).toJsCmd +
                    "; return false;"}>{text}</button>))(_ % _)
  }

  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param jsFunc -- the user function that will be executed. This function will receive as last parameter
   *                  the function that will actually do the ajax call. Hence the user function can decide when
   * 				  to make the ajax request.
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   *
   * @return a button to put on your pagejsFunc.params ++ List(AnonFunc(makeAjaxCall(Str(name+"=true"))))
   */
  def ajaxButton(text: NodeSeq, jsFunc: Call, func: () => JsCmd, attrs: ElemAttr*): Elem = {
    attrs.foldLeft(fmapFunc(contextFuncBuilder(func))(name =>
            <button onclick={deferCall(Str(name + "=true"), jsFunc).toJsCmd + "; return false;"}>{text}</button>))(_ % _)
  }

  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param jsFunc -- the user function that will be executed. This function will receive as last parameter
   *                  the function that will actually do the ajax call. Hence the user function can decide when
   * 				  to make the ajax request.
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   *
   * @return a button to put on your page
   */
  def ajaxButton(text: String, func: () => JsCmd, attrs: ElemAttr*): Elem =
    ajaxButton(Text(text), func, attrs: _*)

  /**
   * Create an Ajax button. When it's pressed, the function is executed
   *
   * @param text -- the name/text of the button
   * @param func -- the function to execute when the button is pushed.  Return Noop if nothing changes on the browser.
   *
   * @return a button to put on your page
   */
  def ajaxButton(text: String, jsFunc: Call, func: () => JsCmd, attrs: ElemAttr*): Elem =
    ajaxButton(Text(text), jsFunc, func, attrs: _*)

  /**
   * This method generates an AJAX editable field. Normally, the displayContents
   * will be shown, with an "Edit" button. If the "Edit" button is clicked, the field
   * will be replaced with the edit form, along with an "OK" and "Cancel" button.
   * If the OK button is pressed, the form fields are submitted and the onSubmit
   * function is called, and then the displayContents are re-run to get a new display.
   * If cancel is pressed then the original displayContents are re-shown.
   */
  def ajaxEditable (displayContents : => NodeSeq, editForm : => NodeSeq, onSubmit : () => JsCmd) : NodeSeq = {
    import _root_.net.liftweb.http.js
    import js.{jquery,JsCmd,JsCmds,JE}
    import jquery.JqJsCmds
    import JsCmds.{Noop,SetHtml}
    import JE.Str
    import JqJsCmds.{Hide,Show}

    val divName = Helpers.nextFuncName
    val dispName = divName + "_display"
    val editName = divName + "_edit"

    def swapJsCmd (show : String, hide : String) : JsCmd = Show(show) & Hide(hide)

    def setAndSwap (show : String, showContents : => NodeSeq, hide : String) : JsCmd =
      (SHtml.ajaxCall(Str("ignore"), {ignore : String => SetHtml(show, showContents)})._2.cmd & swapJsCmd(show,hide))

    def displayMarkup : NodeSeq =
      displayContents ++ Text(" ") ++
      <input value={S.??("edit")} type="button" onclick={setAndSwap(editName, editMarkup, dispName).toJsCmd + " return false;"} />

    def editMarkup : NodeSeq = {
      val formData : NodeSeq =
        editForm ++
        <input type="submit" value={S.??("ok")} /> ++
        hidden(onSubmit) ++
        <input type="button" onclick={swapJsCmd(dispName,editName).toJsCmd + " return false;"} value={S.??("cancel")} />

      ajaxForm(formData,
               Noop,
               setAndSwap(dispName, displayMarkup, editName))
    }

    <div>
      <div id={dispName}>
        {displayMarkup}
      </div>
      <div id={editName} style="display: none;">
        {editMarkup}
      </div>
    </div>
  }

  /**
   * Create an anchor tag around a body which will do an AJAX call and invoke the function
   *
   * @param func - the function to invoke when the link is clicked
   * @param body - the NodeSeq to wrap in the anchor tag
   * @param attrs - the anchor node attributes
   */
  def a(func: () => JsCmd, body: NodeSeq, attrs: ElemAttr*): Elem = {
    val key = formFuncName
    addFunctionMap(key, contextFuncBuilder((a: List[String]) => func()))
    attrs.foldLeft(<lift:a key={key}>{body}</lift:a>)(_ % _)
  }

  /**
   * Create an anchor tag around a body which will do an AJAX call and invoke the function
   *
   * @param jsFunc -- the user function that will be executed. This function will receive as last parameter
   *                  the function that will actually do the ajax call. Hence the user function can decide when
   * 				  to make the ajax request.
   * @param func - the function to invoke when the link is clicked
   * @param body - the NodeSeq to wrap in the anchor tag
   * @param attrs - the anchor node attributes
   */
  def a(jsFunc: Call, func: () => JsCmd, body: NodeSeq, attrs: ElemAttr*): Elem = {
    attrs.foldLeft(fmapFunc(contextFuncBuilder(func))(name =>
            <a href="javascript://" onclick={deferCall(Str(name + "=true"), jsFunc).toJsCmd + "; return false;"}>{body}</a>))(_ % _)
  }

  def a(func: () => JsObj,
        jsonContext: JsonContext,
        body: NodeSeq,
        attrs: ElemAttr*): Elem = {

    attrs.foldLeft(fmapFunc(contextFuncBuilder(func))(name =>
            <a href="javascript://" onclick={makeAjaxCall(Str(name + "=true"), jsonContext).toJsCmd + "; return false;"}>{body}</a>))(_ % _)
  }

  /**
   * Create an anchor with a body and the function to be executed when the anchor is clicked
   */
  def a(body: NodeSeq, attrs: ElemAttr*)(func: => JsCmd): Elem =
    a(() => func, body, attrs: _*)

  /**
   * Create an anchor with a body and the function to be executed when the anchor is clicked
   * @param jsFunc -- the user function that will be executed. This function will receive as last parameter
   *                  the function that will actually do the ajax call. Hence the user function can decide when
   * 				  to make the ajax request.
   * @param body - the NodeSeq to wrap in the anchor tag
   * @param attrs - the anchor node attributes
   */
  def a(jsFunc: Call, body: NodeSeq, attrs: ElemAttr*)(func: => JsCmd): Elem =
    a(jsFunc, () => func, body, attrs: _*)

  /**
   * Create an anchor that will run a JavaScript command when clicked
   */
  def a(body: NodeSeq, cmd: JsCmd, attrs: ElemAttr*): Elem =
    attrs.foldLeft(<a href="javascript://"
    onclick={cmd.toJsCmd + "; return false;"}>{body}</a>)(_ % _)

  /**
   * Create a span that will run a JavaScript command when clicked
   */
  def span(body: NodeSeq, cmd: JsCmd, attrs: ElemAttr*): Elem =
    attrs.foldLeft(<span onclick={cmd.toJsCmd}>{body}</span>)(_ % _)


  def toggleKids(head: Elem, visible: Boolean, func: () => JsCmd, kids: Elem): NodeSeq = {
    fmapFunc(contextFuncBuilder(func)) {
      funcName =>

              val (nk, id) = findOrAddId(kids)
              val rnk = if (visible) nk else nk % ("style" -> "display: none")
              val nh = head %
                      ("onclick" -> (LiftRules.jsArtifacts.toggle(id).cmd & makeAjaxCall(JsRaw("'" + funcName + "=true'")).cmd))
              nh ++ rnk
    }
  }

  /**
   * This function does not really submit a JSON request to server instead json is a function
   * that allows you to build a more complex JsCmd based on the JsExp <i>JE.JsRaw("this.value")</i>.
   * This function is called by the overloaded version of jsonText.
   *
   * @param value - the initial value of the text field
   * @param ignoreBlur - ignore the onblur event and only do the event if the enter key is pressed
   * @param json - takes a JsExp which describes how to recover the
   * value of the text field and returns a JsExp containing the thing
   * to execute on blur/return
   *
   * @return a text field
   */
  def jsonText(value: String, ignoreBlur: Boolean, json: JsExp => JsCmd, attrs: ElemAttr*): Elem = 
  (attrs.foldLeft(<input type="text" value={value}/>)(_ % _)) %
  ("onkeypress" -> """liftUtils.lift_blurIfReturn(event)""") %
  (if (ignoreBlur) Null else ("onblur" -> (json(JE.JsRaw("this.value")))))
  


  /**
   * This function does not really submit a JSON request to server instead json is a function
   * that allows you to build a more complex JsCmd based on the JsExp <i>JE.JsRaw("this.value")</i>.
   * This function is called by the overloaded version of jsonText.
   *
   * @param value - the initial value of the text field
   * @param json - takes a JsExp which describes how to recover the
   * value of the text field and returns a JsExp containing the thing
   * to execute on blur/return
   *
   * @return a text field
   */
  def jsonText(value: String, json: JsExp => JsCmd, attrs: ElemAttr*): Elem = jsonText(value, false, json, attrs :_*)

  /**
   * Create a JSON text widget that makes a JSON call on blur or "return".
   *
   * @param value - the initial value of the text field
   * @param cmd - the json command name
   * @param json - the JsonCall returned from S.buildJsonFunc
   *
   * @return a text field
   */
  def jsonText(value: String, cmd: String, json: JsonCall, attrs: ElemAttr*): Elem =
  jsonText(value, exp => json(cmd, exp), attrs: _*)

  def ajaxTextElem(settable: Settable{type ValueType = String}, attrs: ElemAttr*): Elem =
    ajaxText(settable.get, (b: String) => {settable.set(b); Noop}, attrs :_*)

  def ajaxText(value: String, func: String => JsCmd, attrs: ElemAttr*): Elem = 
  ajaxText_*(value, false, Empty, SFuncHolder(func), attrs: _*)

  def ajaxText(value: String, jsFunc: Call, func: String => JsCmd, attrs: ElemAttr*): Elem = 
  ajaxText_*(value, false, Full(jsFunc), SFuncHolder(func), attrs: _*)

  def ajaxText(value: String, ignoreBlur: Boolean, func: String => JsCmd, attrs: ElemAttr*): Elem =
  ajaxText_*(value, ignoreBlur, Empty, SFuncHolder(func), attrs: _*)

  def ajaxText(value: String, ignoreBlur: Boolean, jsFunc: Call, func: String => JsCmd, attrs: ElemAttr*): Elem =
  ajaxText_*(value, ignoreBlur, Full(jsFunc), SFuncHolder(func), attrs: _*)

  private def ajaxText_*(value: String, ignoreBlur: Boolean, jsFunc: Box[Call], func: AFuncHolder, attrs: ElemAttr*): Elem = {
    val raw = (funcName: String, value: String) => JsRaw("'" + funcName + "=' + encodeURIComponent(" + value + ".value)")
    val key = formFuncName

    fmapFunc(contextFuncBuilder(func)) {
      funcName =>
      (attrs.foldLeft(<input type="text" value={value}/>)(_ % _)) %
      ("onkeypress" -> """liftUtils.lift_blurIfReturn(event)""") %
      (if (ignoreBlur) Null else
       ("onblur" -> (jsFunc match {
              case Full(f) => JsCrVar(key, JsRaw("this")) & deferCall(raw(funcName, key), f)
              case _ => makeAjaxCall(raw(funcName, "this"))
            })
        ))
    }
  }

  /**
   * This function does not really submit a JSON request to server instead json is a function
   * that allows you to build a more complex JsCmd based on the JsExp <i>JE.JsRaw("this.value")</i>.
   * This function is called by the overloaded version of jsonTextarea.
   *
   * @param value - the initial value of the text area field
   * @param json - takes a JsExp which describes how to recover the
   * value of the text area field and returns a JsExp containing the thing
   * to execute on blur
   *
   * @return a text area field
   */
  def jsonTextarea(value: String, json: JsExp => JsCmd, attrs: ElemAttr*): Elem = 
  (attrs.foldLeft(<textarea>{value}</textarea>)(_ % _)) %
  ("onblur" -> (json(JE.JsRaw("this.value"))))
  

  /**
   * Create a JSON text area widget that makes a JSON call on blur
   *
   * @param value - the initial value of the text field
   * @param cmd - the json command name
   * @param json - the JsonCall returned from S.buildJsonFunc
   *
   * @return a text field
   */
  def jsonTextarea(value: String, cmd: String, json: JsonCall, attrs: ElemAttr*): Elem =
  jsonTextarea(value, exp => json(cmd, exp), attrs: _*)

  def ajaxTextarea(value: String, func: String => JsCmd, attrs: ElemAttr*): Elem = 
  ajaxTextarea_*(value, Empty, SFuncHolder(func), attrs: _*)

  def ajaxTextarea(value: String, jsFunc: Call, func: String => JsCmd, attrs: ElemAttr*): Elem = 
  ajaxTextarea_*(value, Full(jsFunc), SFuncHolder(func), attrs: _*)

  private def ajaxTextarea_*(value: String, jsFunc: Box[Call], func: AFuncHolder, attrs: ElemAttr*): Elem = {
    val raw = (funcName: String, value: String) => JsRaw("'" + funcName + "=' + encodeURIComponent(" + value + ".value)")
    val key = formFuncName

    fmapFunc(contextFuncBuilder(func)) {
      funcName =>
      (attrs.foldLeft(<textarea>{value}</textarea>)(_ % _)) %
      ("onblur" -> (jsFunc match {
              case Full(f) => JsCrVar(key, JsRaw("this")) & deferCall(raw(funcName, key), f)
              case _ => makeAjaxCall(raw(funcName, "this"))
            })
        )
    }
  }


  trait AreaShape {
    def shape: String
    def coords: String
  }
  case class RectShape(left: Int, top: Int, right: Int, bottom: Int) extends AreaShape {
    def shape: String = "rect"
    def coords: String = ""+left+", "+top+", "+right+", "+bottom
  }
  case class CircleShape(centerX: Int, centerY: Int, radius: Int) extends AreaShape  {
    def shape: String = "circle"
    def coords: String = ""+centerX+", "+centerY+", "+radius
  }
  case class CirclePercentShape(centerX: Int, centerY: Int, radiusPercent: Int) extends AreaShape  {
    def shape: String = "circle"
    def coords: String = ""+centerX+", "+centerY+", "+radiusPercent+"%"
  }
  case class PolyShape(polyCoords: (Int, Int)*) extends AreaShape {
    def shape: String = "poly"
    def coords: String = polyCoords.map{ case (x, y) => ""+x+", "+y}.mkString(", ")
  }

  /**
   * Generate an Area tag
   *
   * @param shape - the shape of the area (RectShape, CircleShape, CirclePercentShape, PolyShape)
   * @param alt - the contents of the alt attribute
   * @param attrs - the balance of the attributes for the tag
   */
  def area(shape: AreaShape, alt: String, attrs: ElemAttr*): Elem =
  attrs.foldLeft(<area alt={alt} shape={shape.shape} coords={shape.coords} />)(_ % _)

  /**
   * Generate an Area tag
   *
   * @param shape - the shape of the area (RectShape, CircleShape, CirclePercentShape, PolyShape)
   * @param jsCmd - the JavaScript to execute on the client when the area is clicked
   * @param alt - the contents of the alt attribute
   * @param attrs - the balance of the attributes for the tag
   */
  def area(shape: AreaShape, jsCmd: JsCmd, alt: String, attrs: ElemAttr*): Elem =
  area(shape, alt, (("onclick" -> jsCmd.toJsCmd): ElemAttr) :: attrs.toList :_*)

  /**
   * Generate an Area tag
   *
   * @param shape - the shape of the area (RectShape, CircleShape, CirclePercentShape, PolyShape)
   * @param func - The server side function to execute when the area is clicked on.
   * @param alt - the contents of the alt attribute
   * @param attrs - the balance of the attributes for the tag
   */
  def area(shape: AreaShape, func: () => JsCmd, alt: String, attrs: ElemAttr*): Elem = {
    fmapFunc(contextFuncBuilder(func)) {
      funcName =>
      area(shape, alt, (("onclick" -> (makeAjaxCall(Str(funcName + "=true")).toJsCmd +
                                      "; return false;")): ElemAttr) :: attrs.toList :_*)
    }
  }

  def ajaxCheckboxElem(settable: Settable{type ValueType = Boolean}, attrs: ElemAttr*): Elem =
    ajaxCheckbox(settable.get, (b: Boolean) => {settable.set(b); Noop}, attrs :_*)

  def ajaxCheckbox(value: Boolean, func: Boolean => JsCmd, attrs: ElemAttr*): Elem =
    ajaxCheckbox_*(value, Empty, LFuncHolder(in => func(in.exists(toBoolean(_)))), attrs: _*)

  def ajaxCheckboxElem(settable: Settable{type ValueType = Boolean}, jsFunc: Call, attrs: ElemAttr*): Elem =
    ajaxCheckbox_*(settable.get, Full(jsFunc), 
                   LFuncHolder(in => {settable.set(in.exists(toBoolean( _))); 
                                      Noop}), attrs: _*)

  def ajaxCheckbox(value: Boolean, jsFunc: Call, func: Boolean => JsCmd, attrs: ElemAttr*): Elem =
    ajaxCheckbox_*(value, Full(jsFunc), LFuncHolder(in => func(in.exists(toBoolean(_)))), attrs: _*)

  private def ajaxCheckbox_*(value: Boolean, jsFunc: Box[Call], func: AFuncHolder, attrs: ElemAttr*): Elem = {
    val raw = (funcName: String, value: String) => JsRaw("'" + funcName + "=' + " + value + ".checked")
    val key = formFuncName

    fmapFunc(contextFuncBuilder(func)) {
      funcName =>
              (attrs.foldLeft(<input type="checkbox"/>)(_ % _)) %
                      checked(value) %
                      ("onclick" -> (jsFunc match {
                        case Full(f) => JsCrVar(key, JsRaw("this")) & deferCall(raw(funcName, key), f)
                        case _ => makeAjaxCall(raw(funcName, "this"))
                      }))
    }
  }

  /**
   * Make a set of Ajax radio buttons.  When the buttons are pressed,
   * the function is called
   *
   * @param opts -- The possible values.  These are not revealed to the browser
   * @param deflt -- the default button
   * @param ajaxFunc -- the function to invoke when the button is pressed
   */
  def ajaxRadio[T](opts: Seq[T], deflt: Box[T], ajaxFunc: T => JsCmd, attrs: ElemAttr*): ChoiceHolder[T] = {
    val groupName = Helpers.nextFuncName
    val itemList = opts.map{
      v => {
        ChoiceItem(v, attrs.foldLeft(<input type="radio" name={groupName}
                                     value={Helpers.nextFuncName}/>)(_ % _) % 
                   checked(deflt == Full(v)) %
                   ("onclick" -> ajaxCall(Str(""), 
                                          ignore => ajaxFunc(v))._2.toJsCmd))
      }
    }
    ChoiceHolder(itemList)
  }

  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options -- a list of value and text pairs (value, text to display)
   * @param default -- the default value (or Empty if no default value)
   * @param onSubmit -- the function to execute on form submission
   */
  def ajaxSelectElem[T](options: Seq[T], default: Box[T], attrs: ElemAttr*)
  (onSubmit: T => JsCmd)(implicit f: PairStringPromoter[T]):
  Elem = {
    ajaxSelectObj[T](options.map(v => (v -> f(v))),
                     default, onSubmit, attrs :_*)
  }


  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options -- a list of value and text pairs (value, text to display)
   * @param default -- the default value (or Empty if no default value)
   * @param onSubmit -- the function to execute on form submission
   */
  def ajaxSelectObj[T](options: Seq[(T, String)], default: Box[T],
                       onSubmit: T => JsCmd, attrs: ElemAttr*): Elem = {

    val secure = options.map {case (obj, txt) => (obj, randomString(20), txt)}
    val defaultNonce = default.flatMap(d => secure.find(_._1 == d).map(_._2))
    val nonces = secure.map {case (obj, nonce, txt) => (nonce, txt)}
    def process(nonce: String): JsCmd =
      secure.find(_._2 == nonce).map(x => onSubmit(x._1)) getOrElse Noop
    //  (nonces, defaultNonce, SFuncHolder(process))

    ajaxSelect_*(nonces,
      defaultNonce,
      Empty,
      SFuncHolder(process _),
      attrs: _*)
  }

  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options -- a list of value and text pairs (value, text to display)
   * @param default -- the default value (or Empty if no default value)
   * @param onSubmit -- the function to execute on form submission
   */
  def ajaxSelectElem[T](options: Seq[T], default: Box[T],
                        jsFunc: Call,
                        attrs: ElemAttr*)
  (onSubmit: T => JsCmd)
  (implicit f: PairStringPromoter[T]): Elem = 
      {
        ajaxSelectObj[T](options.map(v => (v, f(v))), default,
                         jsFunc, onSubmit)
      }


  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options -- a list of value and text pairs (value, text to display)
   * @param default -- the default value (or Empty if no default value)
   * @param onSubmit -- the function to execute on form submission
   */
  def ajaxSelectObj[T](options: Seq[(T, String)], default: Box[T],
                       jsFunc: Call,
                       onSubmit: T => JsCmd, attrs: ElemAttr*): Elem = {

    val secure = options.map {case (obj, txt) => (obj, randomString(20), txt)}
    val defaultNonce = default.flatMap(d => secure.find(_._1 == d).map(_._2))
    val nonces = secure.map {case (obj, nonce, txt) => (nonce, txt)}
    def process(nonce: String): JsCmd =
      secure.find(_._2 == nonce).map(x => onSubmit(x._1)) getOrElse Noop
    //  (nonces, defaultNonce, SFuncHolder(process))

    ajaxSelect_*(nonces,
      defaultNonce,
      Full(jsFunc),
      SFuncHolder(process _),
      attrs: _*)
  }

  def ajaxSelect(opts: Seq[(String, String)], deflt: Box[String],
                 func: String => JsCmd, attrs: ElemAttr*): Elem =
    ajaxSelect_*(opts, deflt, Empty, SFuncHolder(func), attrs: _*)

  def ajaxSelect(opts: Seq[(String, String)], deflt: Box[String],
                 jsFunc: Call, func: String => JsCmd, attrs: ElemAttr*): Elem =
    ajaxSelect_*(opts, deflt, Full(jsFunc), SFuncHolder(func), attrs: _*)

  private def ajaxSelect_*(opts: Seq[(String, String)], deflt: Box[String],
                           jsFunc: Box[Call], func: AFuncHolder, attrs: ElemAttr*): Elem = {
    val raw = (funcName: String, value: String) => JsRaw("'" + funcName + "=' + " + value + ".options[" + value + ".selectedIndex].value")
    val key = formFuncName

    val vals = opts.map(_._1)
    val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)
    fmapFunc(contextFuncBuilder(testFunc)) {
      funcName =>
              (attrs.foldLeft(<select>{opts.flatMap {case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}}</select>)(_ % _)) %
                      ("onchange" -> (jsFunc match {
                        case Full(f) => JsCrVar(key, JsRaw("this")) & deferCall(raw(funcName, key), f)
                        case _ => makeAjaxCall(raw(funcName, "this"))
                      }))
    }
  }

  def ajaxInvoke(func: () => JsCmd): (String, JsExp) =
    fmapFunc(contextFuncBuilder(NFuncHolder(func)))(name => (name, makeAjaxCall(name + "=true")))

  /**
   * Build a swappable visual element.  If the shown element is clicked on, it turns into the hidden element and when
   * the hidden element blurs, it swaps into the shown element.
   */
  def swappable(shown: Elem, hidden: Elem): Elem = {
    val (rs, sid) = findOrAddId(shown)
    val (rh, hid) = findOrAddId(hidden)
    val ui = LiftRules.jsArtifacts
    (<span>{rs % ("onclick" -> (ui.hide(sid).cmd &
            ui.showAndFocus(hid).cmd & JsRaw("return false;")))}{dealWithBlur(rh % ("style" -> "display: none"), (ui.show(sid).cmd & ui.hide(hid).cmd))}</span>)
  }

  def swappable(shown: Elem, hidden: String => Elem): Elem = {
    val (rs, sid) = findOrAddId(shown)
    val hid = formFuncName
    val ui = LiftRules.jsArtifacts

    val rh = <span id={hid}>{hidden(ui.show(sid).toJsCmd + ";" + ui.hide(hid).toJsCmd + ";")}</span>
    (<span>{rs % ("onclick" -> (ui.hide(sid).toJsCmd + ";" + ui.show(hid).toJsCmd + "; return false;"))}{(rh % ("style" -> "display: none"))}</span>)
  }

  private def dealWithBlur(elem: Elem, blurCmd: String): Elem = {
    (elem \ "@onblur").toList match {
      case Nil => elem % ("onblur" -> blurCmd)
      case x :: xs => val attrs = elem.attributes.filter(_.key != "onblur")
      Elem(elem.prefix, elem.label, new UnprefixedAttribute("onblur", Text(blurCmd + x.text), attrs), elem.scope, elem.child: _*)
    }
  }


  private[http] def appendFuncToURL(url: String, funcStr: String): String =
  splitAtHash(url){to => to + (if (to.indexOf("?") >= 0) "&" else "?") + funcStr}

  private def splitAtHash(str: String)(f: String => String): String =
  str.indexOf("#") match {
    case idx if idx < 0 => f(str)
    case idx => f(str.substring(0, idx)) + str.substring(idx)
  }


  /**
   * create an anchor tag around a body
   *
   * @to - the target
   * @param func - the function to invoke when the link is clicked
   * @param body - the NodeSeq to wrap in the anchor tag
   * @attrs - the (optional) attributes for the HTML element
   */
  def link(to: String, func: () => Any, body: NodeSeq,
           attrs: ElemAttr*): Elem = {
    fmapFunc((a: List[String]) => {func(); true})(key =>
            attrs.foldLeft(<a href={appendFuncToURL(to, key + "=_")}>{body}</a>)(_ % _))
  }

  private def makeFormElement(name: String, func: AFuncHolder,
                              attrs: ElemAttr*): Elem =
    fmapFunc(func)(funcName =>
            attrs.foldLeft(<input type={name} name={funcName}/>)(_ % _))

  def text_*(value: String, func: AFuncHolder, attrs: ElemAttr*): Elem =
    text_*(value, func, Empty, attrs: _*)

  def text_*(value: String, func: AFuncHolder, ajaxTest: String => JsCmd, attrs: ElemAttr*): Elem =
    text_*(value, func, Full(ajaxTest), attrs: _*)

  private def buildOnBlur(bf: Box[String => JsCmd]): MetaData = bf match {
    case Full(func) =>
      new UnprefixedAttribute("onblur", Text(ajaxCall(JsRaw("this.value"), func)._2.toJsCmd), Null)

    case _ => Null
  }


  def text_*(value: String, ignoreBlur: Boolean, func: AFuncHolder, ajaxTest: Box[String => JsCmd], attrs: ElemAttr*): Elem =
    makeFormElement("text", func, attrs: _*) % new UnprefixedAttribute("value", Text(value), Null) % (
      if (ignoreBlur) Null else buildOnBlur(ajaxTest))

  def text_*(value: String, func: AFuncHolder, ajaxTest: Box[String => JsCmd], attrs: ElemAttr*): Elem =
    text_*(value, false, func, ajaxTest, attrs :_*)

  def password_*(value: String, func: AFuncHolder, attrs: ElemAttr*): Elem =
    makeFormElement("password", func, attrs: _*) % ("value" -> value)

  def hidden_*(func: AFuncHolder, attrs: ElemAttr*): Elem =
    makeFormElement("hidden", func, attrs: _*) % ("value" -> "true")

  def submit_*(value: String, func: AFuncHolder, attrs: ElemAttr*): Elem =
    {
      def doit = makeFormElement("submit", func, attrs: _*) % ("value" -> value)

      _formGroup.is match {
        case Empty => formGroup(1)(doit)
        case _ => doit
      }
    }

  private def dupWithName(elem: Elem, name: String): Elem = {
    new Elem(elem.prefix,
             elem.label,
             new UnprefixedAttribute("name", name, 
                                     elem.attributes.filter {
                                       case up: UnprefixedAttribute =>
                                         up.key != "name"
                                       case _ => true
                                     }),
             elem.scope,
             elem.child :_*)
  }

  private def isRadio(in: MetaData): Boolean = 
    in.get("type").map(_.text equalsIgnoreCase "radio") getOrElse false

  private def isCheckbox(in: MetaData): Boolean = 
    in.get("type").map(_.text equalsIgnoreCase "checkbox") getOrElse false

  /**
   * execute the function when the form is submitted.
   * This method returns a function that can be applied to
   * form fields (input, button, textarea, select) and the
   * function is executed when the form containing the field is submitted.
   */
  def onSubmitUnit(func: () => Any): NodeSeq => NodeSeq = 
    onSubmitImpl(func: AFuncHolder)

  /**
   * execute the String function when the form is submitted.
   * This method returns a function that can be applied to
   * form fields (input, button, textarea, select) and the
   * function is executed when the form containing the field is submitted.
   */
  def onSubmit(func: String => Any): NodeSeq => NodeSeq = 
    onSubmitImpl(func: AFuncHolder)

  /**
   * execute the List[String] function when the form is submitted.
   * This method returns a function that can be applied to
   * form fields (input, button, textarea, select) and the
   * function is executed when the form containing the field is submitted.
   */
  def onSubmitList(func: List[String] => Any): NodeSeq => NodeSeq = 
    onSubmitImpl(func: AFuncHolder)

  /**
   * Execute the Boolean function when the form is submitted.
   * This method returns a function that can be applied to
   * form fields (input, button, textarea, select) and the
   * function is executed when the form containing the field is submitted.
   */
  def onSubmitBoolean(func: Boolean => Any): NodeSeq => NodeSeq = 
    onSubmitImpl(func: AFuncHolder)

  /**
   * Execute the function when the form is submitted.
   * This method returns a function that can be applied to
   * form fields (input, button, textarea, select) and the
   * function is executed when the form containing the field is submitted.
   */
  def onSubmitImpl(func: AFuncHolder): NodeSeq => NodeSeq =
    (in: NodeSeq) => {
      var radioName: Box[String] = Empty
      var checkBoxName: Box[String] = Empty
      var checkBoxCnt = 0

      def runNodes(in: NodeSeq): NodeSeq = 
        in.flatMap {
          case Group(g) => runNodes(g)
          // button
          case e: Elem if e.label == "button" => 
            fmapFunc(func) {dupWithName(e, _)}

          // textarea
          case e: Elem if e.label == "textarea" => 
            fmapFunc(func) {dupWithName(e, _)}

          // select
          case e: Elem if e.label == "select" => 
            fmapFunc(func) {dupWithName(e, _)}

          // radio
          case e: Elem if e.label == "input" && isRadio(e.attributes) => 
            radioName match {
              case Full(name) => dupWithName(e, name)
              case _ =>
                fmapFunc(func) {
                  name => {
                    radioName = Full(name)
                    dupWithName(e, name)
                  }
                }
            }
          
          // checkbox
          case e: Elem if e.label == "input" && isCheckbox(e.attributes) => 
            checkBoxName match {
              case Full(name) =>
                checkBoxCnt += 1
                dupWithName(e, name)
              case _ =>
                fmapFunc(func) {
                  name => {
                    checkBoxName = Full(name)
                    checkBoxCnt += 1
                    dupWithName(e, name)
                  }
                }
            }

          // generic input
          case e: Elem if e.label == "input" => 
            fmapFunc(func) {dupWithName(e, _)}

          case x => x
        }

      val ret = runNodes(in)
      
      checkBoxName match {
        // if we've got a single checkbox, add a hidden false checkbox
        case Full(name) if checkBoxCnt == 1 => {
          ret ++ <input type="hidden" name={name} value="false"/> 
        }

        case _ => ret
      }
    }

  def text(value: String, func: String => Any, attrs: ElemAttr*): Elem =
    text_*(value, SFuncHolder(func), attrs: _*)

  /**
   * Generate an input element for the Settable
   */
  def textElem(settable: Settable{type ValueType = String}, attrs: ElemAttr*): Elem =
    text_*(settable.get, SFuncHolder(s => settable.set(s)), attrs: _*)

  /**
   * Generate an input field with type email.  At some point,
   * there will be graceful fallback for non-HTML5 browsers.  FIXME
   */
  def email(value: String, func: String => Any, attrs: ElemAttr*): Elem =
    email_*(value, SFuncHolder(func), attrs: _*)

  /**
   * Generate an email input element for the Settable. At some point
   * there will be graceful fallback for non-HTML5 browsers. FIXME
   */
  def email(settable: Settable{type ValueType = String},
            attrs: ElemAttr*): Elem =
              email_*(settable.get, SFuncHolder(s => settable.set(s)), attrs: _*)

  private def email_*(value: String, func: AFuncHolder, attrs: ElemAttr*): Elem =
    makeFormElement("email", func, attrs: _*) %
  new UnprefixedAttribute("value", Text(value), Null)

  /**
   * Generate an input field with type url.  At some point,
   * there will be graceful fallback for non-HTML5 browsers.  FIXME
   */
  def url(value: String, func: String => Any, attrs: ElemAttr*): Elem =
    url_*(value, SFuncHolder(func), attrs: _*)

  /**
   * Generate a url input element for the Settable. At some point
   * there will be graceful fallback for non-HTML5 browsers. FIXME
   */
  def url(settable: Settable{type ValueType = String},
          attrs: ElemAttr*): Elem =
            url_*(settable.get, SFuncHolder(s => settable.set(s)), attrs: _*)

  private def url_*(value: String, func: AFuncHolder, attrs: ElemAttr*): Elem =
    makeFormElement("url", func, attrs: _*) %
  new UnprefixedAttribute("value", Text(value), Null)

  /**
   * Generate an input field with type number.  At some point,
   * there will be graceful fallback for non-HTML5 browsers.  FIXME
   */
  def number(value: Int, func: Int => Any,
             min: Int, max: Int, attrs: ElemAttr*): Elem =
    number_*(value, 
             min, max,
             SFuncHolder(s => Helpers.asInt(s).map(func)), attrs: _*)

  /**
   * Generate a number input element for the Settable. At some point
   * there will be graceful fallback for non-HTML5 browsers. FIXME
   */
  def number(settable: Settable{type ValueType = Int},
             min: Int, max: Int,
             attrs: ElemAttr*): Elem =
               number_*(settable.get, min, max,
                        SFuncHolder(s => Helpers.asInt(s).map(s => settable.set(s))),
                        attrs: _*)
  
  private def number_*(value: Int,
                       min: Int, max: Int,
                       func: AFuncHolder, attrs: ElemAttr*): Elem = {
    import Helpers._

    makeFormElement("number", 
                    func,
                    attrs: _*) % 
    ("value" -> value.toString) %
    ("min" -> min.toString) %  
    ("max" -> max.toString)
  }

  /**
   * Generate an input field with type range.  At some point,
   * there will be graceful fallback for non-HTML5 browsers.  FIXME
   */
  def range(value: Int, func: Int => Any,
             min: Int, max: Int, attrs: ElemAttr*): Elem =
    range_*(value, 
            min, max,
            SFuncHolder(s => Helpers.asInt(s).map(func)), attrs: _*)

  /**
   * Generate a range input element for the Settable. At some point
   * there will be graceful fallback for non-HTML5 browsers. FIXME
   */
  def range(settable: Settable{type ValueType = Int},
            min: Int, max: Int,
             attrs: ElemAttr*): Elem =
               range_*(settable.get, min, max,
                       SFuncHolder(s => Helpers.asInt(s).map(s => settable.set(s))),
                       attrs: _*)
  
  private def range_*(value: Int,
                      min: Int, max: Int,
                      func: AFuncHolder, attrs: ElemAttr*): Elem = {
    import Helpers._

    makeFormElement("range", 
                    func,
                    attrs: _*) % 
    ("value" -> value.toString) %
    ("min" -> min.toString) %  
    ("max" -> max.toString)
  }




  def textAjaxTest(value: String, func: String => Any, ajaxTest: String => JsCmd, attrs: ElemAttr*): Elem =
    text_*(value, SFuncHolder(func), ajaxTest, attrs: _*)

  def textAjaxTest(value: String, func: String => Any, ajaxTest: Box[String => JsCmd], attrs: ElemAttr*): Elem =
    text_*(value, SFuncHolder(func), ajaxTest, attrs: _*)


  def password(value: String, func: String => Any, attrs: ElemAttr*): Elem =
    makeFormElement("password", SFuncHolder(func), attrs: _*) % new UnprefixedAttribute("value", Text(value), Null)

  def passwordElem(settable: Settable{type ValueType = String}, attrs: ElemAttr*): Elem =
    makeFormElement("password", SFuncHolder(s => settable.set(s)), attrs: _*) % new UnprefixedAttribute("value", Text(settable.get), Null)

  def hidden(func: () => Any, attrs: ElemAttr*): Elem =
    makeFormElement("hidden", NFuncHolder(func), attrs: _*) % ("value" -> "true")

  def hidden(func: (String) => Any, defaultlValue: String, attrs: ElemAttr*): Elem =
    makeFormElement("hidden", SFuncHolder(func), attrs: _*) % ("value" -> defaultlValue)

  /**
   * Create an HTML button with strOrNodeSeq as the body.  The
   * button will be type submit.
   *
   * @param strOrNodeSeq -- the String or NodeSeq (either will work just fine)
   * to put into the body of the button
   * @param func -- the function to execute when the form containing the button
   * is posted
   * @param attrs -- the attributes to append to the button
   * @return a button HTML Element b
   */
  def button(strOrNodeSeq: StringOrNodeSeq, func: () => Any, attrs: ElemAttr*): Elem = {
    def doit: Elem = {
      attrs.foldLeft(fmapFunc(contextFuncBuilder(func))(name =>
        <button type="submit" name={name} value="_">{
          strOrNodeSeq.nodeSeq}</button>))(_ % _)
    }

    _formGroup.is match {
      case Empty => formGroup(1)(doit)
      case _ => doit
    }
  }
             
  /**
   * Generates a form submission button.
   *
   * @param value The label for the button
   * @param func The function that will be executed on form submission
   * @param attrs Optional XHTML element attributes that will be applied to the button
   */
  def submit(value: String, func: () => Any, attrs: ElemAttr*): Elem = {

    def doit = {
      makeFormElement("submit", NFuncHolder(func), attrs: _*) %
              new UnprefixedAttribute("value", Text(value), Null)
    }

    _formGroup.is match {
      case Empty => formGroup(1)(doit)
      case _ => doit
    }
  }

  /**
   * Constructs an Ajax submit button that can be used inside ajax forms.
   * Multiple buttons can be used in the same form.
   * 
   * @param value - the button text
   * @param func - the ajax function to be called
   * @param attrs - button attributes
   *
   */
  def ajaxSubmit(value: String, func: () => JsCmd, attrs: ElemAttr*): Elem = {
    val funcName = "z" + Helpers.nextFuncName
    addFunctionMap(funcName, contextFuncBuilder(func))

    (attrs.foldLeft(<input type="submit" name={funcName}/>)(_ % _)) %
      new UnprefixedAttribute("value", Text(value), Null) %
      ("onclick" -> ("liftAjax.lift_uriSuffix = '"+funcName+"=_'; return true;"))
    
  }

  /**
   * Generates a form submission button with a default label.
   *
   * @param func The function that will be executed on form submission
   * @param attrs Optional XHTML element attributes that will be applied to the button
   */
  def submitButton(func: () => Any, attrs: ElemAttr*): Elem = makeFormElement("submit", NFuncHolder(func), attrs: _*)

  /**
   * Takes a form and wraps it so that it will be submitted via AJAX.
   *
   * @param body The form body. This should not include the &lt;form&gt; tag.
   */
  def ajaxForm(body: NodeSeq) = (<lift:form>{body}</lift:form>)

  /**
   * Takes a form and wraps it so that it will be submitted via AJAX.
   *
   * @param body The form body. This should not include the &lt;form&gt; tag.
   * @param onSubmit JavaScript code to execute on the client prior to submission
   *
   * @deprecated Use ajaxForm(NodeSeq,JsCmd) instead
   */
  def ajaxForm(onSubmit: JsCmd, body: NodeSeq) = (<lift:form onsubmit={onSubmit.toJsCmd}>{body}</lift:form>)

  /**
   * Takes a form and wraps it so that it will be submitted via AJAX.
   *
   * @param body The form body. This should not include the &lt;form&gt; tag.
   * @param onSubmit JavaScript code to execute on the client prior to submission
   */
  def ajaxForm(body: NodeSeq, onSubmit: JsCmd) = (<lift:form onsubmit={onSubmit.toJsCmd}>{body}</lift:form>)

  /**
   * Takes a form and wraps it so that it will be submitted via AJAX. This also
   * takes a parameter for script code that will be executed after the form has been submitted.
   *
   * @param body The form body. This should not include the &lt;form&gt; tag.
   * @param postSubmit Code that should be executed after a successful submission
   */
  def ajaxForm(body : NodeSeq, onSubmit : JsCmd, postSubmit : JsCmd) = (<lift:form onsubmit={onSubmit.toJsCmd} postsubmit={postSubmit.toJsCmd}>{body}</lift:form>)

  /**
   * Takes a form and wraps it so that it will be submitted via AJAX and processed by
   * a JSON handler. This can be useful if you may have dynamic client-side modification
   * of the form (addition or removal).
   *
   * @param jsonHandler The handler that will process the form
   * @param body The form body. This should not include the &lt;form&gt; tag.
   */
  def jsonForm(jsonHandler: JsonHandler, body: NodeSeq): NodeSeq = jsonForm(jsonHandler, Noop, body)

  /**
   * Takes a form and wraps it so that it will be submitted via AJAX and processed by
   * a JSON handler. This can be useful if you may have dynamic client-side modification
   * of the form (addition or removal).
   *
   * @param jsonHandler The handler that will process the form
   * @param onSubmit JavaScript code that will be executed on the client prior to submitting
   * the form
   * @param body The form body. This should not include the &lt;form&gt; tag.
   */
  def jsonForm(jsonHandler: JsonHandler, onSubmit: JsCmd, body: NodeSeq): NodeSeq = {
    val id = formFuncName
    <form onsubmit={(onSubmit & jsonHandler.call("processForm", FormToJSON(id)) & JsReturn(false)).toJsCmd} id={id}>{body}</form>
  }

  /**
   * Having a regular form, this method can be used to send the content of the form as JSON.
   * the request will be processed by the jsonHandler
   *
   * @param jsonHandler - the handler that process this request
   * @oaram formId - the id of the form
   */
  def submitJsonForm(jsonHandler: JsonHandler, formId: String):JsCmd = jsonHandler.call("processForm", FormToJSON(formId))

  /**
   * Having a regular form, this method can be used to send the serialized content of the form.
   *
   * @oaram formId - the id of the form
   */
  def submitAjaxForm(formId: String):JsCmd = SHtml.makeAjaxCall(LiftRules.jsArtifacts.serialize(formId))

  /** 
   * Submits a form denominated by a formId and execute the func function
   * after form fields functions are executed.
   */ 
  def submitAjaxForm(formId: String, func: () => JsCmd): JsCmd = {
    val funcName = "Z" + Helpers.nextFuncName
    addFunctionMap(funcName, contextFuncBuilder(func))

    makeAjaxCall(JsRaw(
       LiftRules.jsArtifacts.serialize(formId).toJsCmd + " + " + 
       Str("&" + funcName + "=true").toJsCmd))
  }

  /**
   * Having a regular form, this method can be used to send the serialized content of the form.
   *
   * @oaram formId - the id of the form
   * @param postSubmit - the function that needs to be called after a successfull request
   */
  def submitAjaxForm(formId: String, postSubmit: Call):JsCmd =
    SHtml.makeAjaxCall(LiftRules.jsArtifacts.serialize(formId), AjaxContext.js(Full(postSubmit.toJsCmd)))


  private def secureOptions[T](options: Seq[(T, String)], default: Box[T],
                               onSubmit: T => Any): (Seq[(String, String)], Box[String], AFuncHolder) = {
    val secure = options.map {case (obj, txt) => (obj, randomString(20), txt)}
    val defaultNonce = default.flatMap(d => secure.find(_._1 == d).map(_._2))
    val nonces = secure.map {case (obj, nonce, txt) => (nonce, txt)}
    def process(nonce: String): Unit =
      secure.find(_._2 == nonce).map(x => onSubmit(x._1))
    (nonces, defaultNonce, SFuncHolder(process))
  }

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission
   *
   * @param opts -- the options.  A list of value and text pairs (value, text to display)
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def select(opts: Seq[(String, String)], deflt: Box[String], func: String => Any, attrs: ElemAttr*): Elem =
    select_*(opts, deflt, SFuncHolder(func), attrs: _*)

  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options -- a list of values
   * @param default -- the default value (or Empty if no default value)
   * @param attrs -- the attributes to append to the resulting Elem,
   * these may be name-value pairs (static attributes) or special
   * HTML5 ElemAtts
   * @param onSubmit -- the function to execute on form submission
   * @param f -- the function that converts a T to a Display String.
   */
  def selectElem[T](options: Seq[T], default: Box[T], attrs: ElemAttr*)
  (onSubmit: T => Any)
  (implicit f: PairStringPromoter[T]): 
  Elem = {
    selectObj[T](options.map(v => (v, f(v))), default, onSubmit, attrs :_*)
  }

  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options -- a list of values
   * @param default -- the default value (or Empty if no default value)
   * @param attrs -- the attributes to append to the resulting Elem,
   * these may be name-value pairs (static attributes) or special
   * HTML5 ElemAtts
   * @param onSubmit -- the function to execute on form submission
   * @param f -- the function that converts a T to a Display String.
   */
  def selectElem[T](options: Seq[T], 
                    settable: LiftValue[T], 
                    attrs: ElemAttr*)
  (implicit f: PairStringPromoter[T]): 
  Elem = {
    selectObj[T](options.map(v => (v, f(v))), Full(settable.get), 
                 s => settable.set(s), attrs :_*)
  }

  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options -- a list of value and text pairs (value, text to display)
   * @param default -- the default value (or Empty if no default value)
   * @param onSubmit -- the function to execute on form submission
   */
  def selectObj[T](options: Seq[(T, String)], default: Box[T],
                   onSubmit: T => Any, attrs: ElemAttr*): Elem = {
    val (nonces, defaultNonce, secureOnSubmit) =
    secureOptions(options, default, onSubmit)

    select_*(nonces, defaultNonce, secureOnSubmit, attrs: _*)
  }

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def select_*(opts: Seq[(String, String)], deflt: Box[String],
               func: AFuncHolder, attrs: ElemAttr*): Elem = {
    val vals = opts.map(_._1)
    val testFunc = LFuncHolder(in => in.filter(v => vals.contains(v)) match {case Nil => false case xs => func(xs)}, func.owner)

    attrs.foldLeft(fmapFunc(testFunc)(fn => <select name={fn}>{opts.flatMap {case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}}</select>))(_ % _)
  }

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission.  No check is made to see if the resulting value was in the original list.
   * For use with DHTML form updating.
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def untrustedSelect(opts: Seq[(String, String)], deflt: Box[String],
                      func: String => Any, attrs: ElemAttr*): Elem =
    untrustedSelect_*(opts, deflt, SFuncHolder(func), attrs: _*)

  /**
   * Create a select box based on the list with a default value and the function to be executed on
   * form submission.  No check is made to see if the resulting value was in the original list.
   * For use with DHTML form updating.
   *
   * @param opts -- the options.  A list of value and text pairs
   * @param deflt -- the default value (or Empty if no default value)
   * @param func -- the function to execute on form submission
   */
  def untrustedSelect_*(opts: Seq[(String, String)], deflt: Box[String],
                        func: AFuncHolder, attrs: ElemAttr*): Elem =
    fmapFunc(func)(funcName =>
            attrs.foldLeft(<select name={funcName}>{opts.flatMap {case (value, text) => (<option value={value}>{text}</option>) % selected(deflt.exists(_ == value))}}</select>)(_ % _))


  private def selected(in: Boolean) = if (in) new UnprefixedAttribute("selected", "selected", Null) else Null

  def multiSelect(opts: Seq[(String, String)], deflt: Seq[String],
                  func: List[String] => Any, attrs: ElemAttr*): Elem =
    multiSelect_*(opts, deflt, LFuncHolder(func), attrs: _*)

  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options -- a list of value and text pairs (value, text to display)
   * @param default -- the default value (or Empty if no default value)
   * @param onSubmit -- the function to execute on form submission
   */
  def multiSelectElem[T](options: Seq[T], default: Seq[T], attrs: ElemAttr*)
  (onSubmit: List[T] => Any)
  (implicit f: PairStringPromoter[T]): Elem = {
    multiSelectObj[T](options.map(v => (v, f(v))), default, 
                      onSubmit, attrs :_*)
  }

  /**
   * Create a select box based on the list with a default value and the function
   * to be executed on form submission
   *
   * @param options -- a list of value and text pairs (value, text to display)
   * @param default -- the default value (or Empty if no default value)
   * @param onSubmit -- the function to execute on form submission
   */
  def multiSelectObj[T](options: Seq[(T, String)], default: Seq[T],
                        onSubmit: List[T] => Any, attrs: ElemAttr*): Elem = {
    val (nonces, defaultNonce, secureOnSubmit) =
    secureMultiOptions(options, default, onSubmit)

    multiSelect_*(nonces, defaultNonce, secureOnSubmit, attrs: _*)
  }

  private[http] def secureMultiOptions[T](options: Seq[(T, String)], default: Seq[T],
                                          onSubmit: List[T] => Any): (Seq[(String, String)],
          Seq[String], AFuncHolder) =
    {
      val o2 = options.toList

      val secure: List[(T, String, String)] = o2.map {case (obj, txt) => (obj, randomString(20), txt)}
      val sm: Map[String, T] = Map(secure.map(v => (v._2, v._1)): _*)
      val defaultNonce: Seq[String] = default.flatMap(d => secure.find(_._1 == d).map(_._2))
      val nonces: List[(String, String)] = secure.map {case (obj, nonce, txt) => (nonce, txt)}.toList
      def process(info: List[String]): Unit = onSubmit(info.flatMap(sm.get))

      (nonces, defaultNonce, LFuncHolder(process))
    }

  def multiSelect_*(opts: Seq[(String, String)],
                    deflt: Seq[String],
                    func: AFuncHolder, attrs: ElemAttr*): Elem =
    fmapFunc(func)(funcName =>
            attrs.foldLeft(<select multiple="true" name={funcName}>{opts.flatMap(o => (<option value={o._1}>{o._2}</option>) % selected(deflt.contains(o._1)))}</select>)(_ % _))


  def textarea(value: String, func: String => Any, attrs: ElemAttr*): Elem =
    textarea_*(value, SFuncHolder(func), attrs: _*)

  def textareaElem(settable: Settable{type ValueType = String}, 
                   attrs: ElemAttr*):
  Elem = textarea_*(settable.get, SFuncHolder(s => settable.set(s)), attrs: _*)

  def textarea_*(value: String, func: AFuncHolder, attrs: ElemAttr*): Elem =
    fmapFunc(func)(funcName =>
            attrs.foldLeft(<textarea name={funcName}>{value}</textarea>)(_ % _))

  def radio(opts: Seq[String], deflt: Box[String], func: String => Any,
            attrs: ElemAttr*): ChoiceHolder[String] =
    radio_*(opts, deflt, SFuncHolder(func), attrs: _*)

  /**
   * Generate a collection or radio box items from a sequence of
   * things
   */
  def radioElem[T](opts: Seq[T], deflt: Box[T], attrs: ElemAttr*)
  (onSubmit: Box[T] => Any): ChoiceHolder[T] = {
    val possible = opts.map(v => Helpers.nextFuncName -> v).toList

    val hiddenId = Helpers.nextFuncName

    fmapFunc(LFuncHolder(lst => lst.filter(_ != hiddenId) match {
      case Nil => onSubmit(Empty)
      case x :: _ => onSubmit(possible.filter(_._1 == x).
                              headOption.map(_._2))
    })) {
      name => {
        val items = possible.zipWithIndex.map {
          case ((id, value), idx) => {
            val radio = 
              attrs.foldLeft(<input type="radio"
                             name={name} value={id}/>)(_ % _) %
            checked(deflt.filter(_ == value).isDefined)

            val elem = if (idx == 0) {
              radio ++ <input type="hidden" value={hiddenId} name={name}/>
            } else {
              radio
            }
            
            ChoiceItem(value, elem)
          }
        }
        
        ChoiceHolder(items)
      }
    }
  }

  def radio_*(opts: Seq[String], deflt: Box[String],
              func: AFuncHolder, attrs: ElemAttr*): ChoiceHolder[String] = {
    fmapFunc(func) {
      name =>
              val itemList = opts.map(v => ChoiceItem(v,
                attrs.foldLeft(<input type="radio" name={name} value={v}/>)(_ % _) %
                        checked(deflt.filter((s: String) => s == v).isDefined)))
              ChoiceHolder(itemList)
    }
  }

  def fileUpload(func: FileParamHolder => Any, attrs: ElemAttr*): Elem = {
    val f2: FileParamHolder => Any =
      fp => if (fp.file != null && fp.file.length > 0) func(fp)
    fmapFunc(BinFuncHolder(f2)) { name => 
      attrs.foldLeft(<input type="file" name={ name }/>) { _ % _ }
    }
  }

  /** Holds a form control as HTML along with some user defined value */
  final case class ChoiceItem[T](key: T, xhtml: NodeSeq)

  /** Holds a series of choices: HTML for input controls alongside some user defined value */
  final case class ChoiceHolder[T](items: Seq[ChoiceItem[T]]) {
    /** Retrieve the ChoiceItem that has the given key, throwing NoSuchElementException if there is no matching ChoiceItem */
    def apply(in: T): NodeSeq = items.filter(_.key == in).first.xhtml

    /** Retrieve the nth ChoiceItem, 0-based */
    def apply(in: Int): NodeSeq = items(in).xhtml

    /** Apply a function to each ChoiceItem, collecting the results */
    def map[A](f: ChoiceItem[T] => A) = items.map(f)

    /** Apply a function to each ChoiceItem, concatenating the results */
    def flatMap[A](f: ChoiceItem[T] => Iterable[A]) = items.flatMap(f)

    /** Return the ChoiceItems that the given function returns true for */
    def filter(f: ChoiceItem[T] => Boolean) = items.filter(f)

    /** Generate a simple form by calling ChoiceItem.htmlize on each ChoiceItem and concatenating the resulting HTML */
    def toForm: NodeSeq = flatMap(ChoiceHolder.htmlize)
  }

   
  object ChoiceHolder {
    /** Convert a ChoiceItem into a span containing the control and the toString of the key */
    var htmlize: ChoiceItem[_] => NodeSeq = c => (<span>{c.xhtml}&nbsp;{c.key.toString}<br/> </span>)
  }

  private def checked(in: Boolean) = if (in) new UnprefixedAttribute("checked", "checked", Null) else Null

  private def setId(in: Box[String]) = in match {case Full(id) => new UnprefixedAttribute("id", Text(id), Null); case _ => Null}

  /**
   * Generate a ChoiceHolder of possible checkbox type inputs that calls back to the given function when the form is submitted.
   *
   * @param possible complete sequence of possible values, each a separate checkbox when rendered
   * @param actual values to be preselected
   * @param func function to receive all values corresponding to the checked boxes
   * @param attrs sequence of attributes to apply to each checkbox input element
   * @return ChoiceHolder containing the checkboxes and values in order
   */
  def checkbox[T](possible: Seq[T], actual: Seq[T], func: Seq[T] => Any, attrs: ElemAttr*): ChoiceHolder[T] = {
    val len = possible.length
    fmapFunc(LFuncHolder((strl: List[String]) => {func(strl.map(toInt(_)).filter(x => x >= 0 && x < len).map(possible(_))); true})) {
      name =>
              ChoiceHolder(possible.toList.zipWithIndex.map(p =>
                      ChoiceItem(p._1,
                        attrs.foldLeft(<input type="checkbox" name={name} value={p._2.toString}/>)(_ % _) %
                                checked(actual.contains(p._1)) ++ (if (p._2 == 0) (<input type="hidden" name={name} value="-1"/>) else Nil))))
    }
  }

  /**
   * Defines a new checkbox for the Settable
   */
  def checkboxElem(settable: Settable{type ValueType = Boolean}, attrs: ElemAttr*): NodeSeq = {
    checkbox_id(settable.get, s => settable.set(s), Empty, attrs: _*)
  }

  /**
   * Defines a new checkbox set to  { @code value } and running  { @code func } when the
   * checkbox is submitted.
   */
  def checkbox(value: Boolean, func: Boolean => Any, attrs: ElemAttr*): NodeSeq = {
    checkbox_id(value, func, Empty, attrs: _*)
  }

  /**
   * Defines a new checkbox for the Settable
   */
  def checkbox_id(settable: Settable{type ValueType = Boolean},
                  id: Box[String], attrs: ElemAttr*): NodeSeq = {
    def from(f: Boolean => Any): List[String] => Boolean = (in: List[String]) => {
      f(in.exists(toBoolean(_)))
      true
    }
    checkbox_*(settable.get, LFuncHolder(from(s => settable.set(s))), id, attrs: _*)
  }

  /**
   * Defines a new checkbox set to  { @code value } and running  { @code func } when the
   * checkbox is submitted. Has an id of  { @code id }.
   */
  def checkbox_id(value: Boolean, func: Boolean => Any,
                  id: Box[String], attrs: ElemAttr*): NodeSeq = {
    def from(f: Boolean => Any): List[String] => Boolean = (in: List[String]) => {
      f(in.exists(toBoolean(_)))
      true
    }
    checkbox_*(value, LFuncHolder(from(func)), id, attrs: _*)
  }

  def checkbox_*(value: Boolean, func: AFuncHolder, id: Box[String],
                 attrs: ElemAttr*): NodeSeq = {
    fmapFunc(func)(name =>
            (<input type="hidden" name={name} value="false"/>) ++
                    (attrs.foldLeft(<input type="checkbox" name={name} value="true"/>)(_ % _) % checked(value) % setId(id))
      )
  }

}

object AjaxType extends Enumeration("javascript", "json") {
  val JavaScript, JSON = Value
}

object AjaxContext {
  def js(success: Box[String], failure: Box[String]) = new JsContext(success, failure)

  def js(success: Box[String]) = new JsContext(success, Empty)

  def json(success: Box[String], failure: Box[String]) = new JsonContext(success, failure)

  def json(success: Box[String]) = new JsonContext(success, Empty)
}


case class AjaxContext(success: Box[String], failure: Box[String], responseType: AjaxType.Value)

case class JsContext(override val success: Box[String], override val failure: Box[String]) extends AjaxContext(success, failure, AjaxType.JavaScript)

case class JsonContext(override val success: Box[String], override val failure: Box[String]) extends AjaxContext(success, failure, AjaxType.JSON)

object Html5ElemAttr {
  /**
   * The autofocus attribute
   */
  final case object Autofocus extends SHtml.ElemAttr {
    // FIXME detect HTML5 browser and do the right thing
    def apply(in: Elem): Elem = in % ("autofocus" -> "true")
  }

  /**
   * The required attribute
   */
  final case object Required extends SHtml.ElemAttr {
    // FIXME detect HTML5 browser and do the right thing
    def apply(in: Elem): Elem = in % ("required" -> "true")
  }

  /**
   * The placeholder attribute for HTML5.
   *
   * @param text - a String or () => String that will be the
   * placeholder property in the attribute
   */
  final case class Placeholder(text: StringFunc) extends SHtml.ElemAttr {
    // FIXME detect HTML5 browser and do the right thing
    def apply(in: Elem): Elem = in % ("placeholder" -> text.func())
  }
}

/**
 * Mix this trait into a snippet class so that you have a convenient
 * value to redirect back to (whence).
 * When you're done with the snippet, <code>S.redirectTo(whence)</code>
 */
trait Whence {
  protected val whence = S.referer openOr "/"
}

}
}
