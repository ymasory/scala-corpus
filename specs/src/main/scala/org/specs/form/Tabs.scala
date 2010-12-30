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
package org.specs.form
import org.specs.xml.Xhtml._
import org.specs.xml.NodeFunctions._
import org.specs.util.ExtendedString._
import org.specs.util.Classes._

/**
 * This trait adds Tabulations to a table
 */
trait Tabs extends Layout { outer => 
  /** case class for creating a group of tabs */
  case class tabs() extends Layoutable with LabeledXhtml {
    val label = className(this.getClass).uncamel
    /** adds the group of tabs to the Layout on a new row */
    outer.tr(this)
    var tabValues: List[tab] = Nil
    override def toXhtml = {
      <div class="tabber">{reduce(tabValues.reverse, ((_:ToXhtml).toXhtml))}</div>
    }
    private def addTab(t: tab) = { tabValues = t :: tabValues; this }

    /** case class for creating a tab */
    case class tab(title: String) extends Layoutable {
      /** add the tab to the group of tabs */
      addTab(this)
      /** @return the xhtml in a special div for the tabulation */
      override def toXhtml = {
        <div class="tabbertab" title={title}><table class="dataTable">{spanLastTd(super.xhtml)}</table></div>
      }
    }
  }
  /**
   * create tabs with an overall title from a list of forms
   */
  def tabs[T <: Form](title: String, formsToTab: T*): Form = toTabs(title, formsToTab.map(f => (f.title, f)):_*)
  /**
   * create tabs with an overall title from a list of (tab title, form)
   */
  def toTabs[T <: Form](title: String, formsToTab: (String, T)*): Form = {
    new Form(title) {
      new tabs {
        formsToTab foreach { tabTitleAndForm => val (t, f) = tabTitleAndForm
          new tab(t) {
            trs(f.rows)
          }
        }
      }
    }
  }

}
