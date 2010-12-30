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
import org.specs.form._
import org.specs.execute._
/**
 * This trait adds shortcut to declare forms in the specification text
 */
trait LiterateForms extends BaseSpecification with ExpectableFactory { outer =>
  /**
   * This method allows to embbed a Form in a literate specification and display the results of its execution
   */
  implicit def makeForm(s: String) = new LiterateForm(s)
  case class LiterateForm(desc: String) {
    def inForm(form: =>Form) = {
      lazy val formToExecute = form
      val description = if (desc.isEmpty) form.title else desc
      forExample(description) in {
          isExpectation(formToExecute.execute)
          if (!formToExecute.isOk) throw new FailureException("The form '" +  formToExecute.title + "' failed")
      }
      description + "\n" + formToExecute.toHtml.toString
    }
  }
  implicit def toReportableForm(f: Form) = new ReportableForm(f)
  class ReportableForm(f: Form) {
    def report = f.reportTo(outer)
  }
}
