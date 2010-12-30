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
import org.specs.util._
import org.specs.xml.NodeFunctions._
import scala.collection.mutable.ListBuffer
import org.specs.form._

class DataTableForm(title: Option[String]) extends TableForm(title) with DataTableFormEnabled {
  def this() = this(None)
  def this(t: String) = this(Some(t))
}
trait DataTableFormEnabled extends TableFormEnabled with DataTables {
  private var unsetHeader = true
  /** header retrieved from the DataTable header */
  protected var tableHeader: Option[TableHeader] = None
  /** store a reference to the DataTable header */
  implicit override def toTableHeader(s: String) = {
    val th = super.toTableHeader(s)
    tableHeader = Some(th)
    th
  }
  /** @return the data table if the header is set */
  def dataTable: Option[ExecutableDataTable] = tableHeader.flatMap(_.table)
  /** add a header row if it hasn't been done */
  override def tr[F <: Form](line: F): F = {
    line match {
      case l: LineForm => setHeader(l)
      case _ => ()
    }
    appendRows(line.rows)
    line
  }
  override def setHeader[F <: LineForm](line: F): F = {
    if (unsetHeader && tableHeader.isDefined) {
      tableHeader.map((header: TableHeader) => inNewRow(reduce(header.titles, { (s: String) => <th>{s}</th> })))
      unsetHeader = false
    }
    line
  }
  /** execute the table to create the properties and execute them */
  override def execute = {
    if (!executed) {
      executeTable
      super.execute
    }
    this
  }
  protected def executeTable = {
    tableHeader.map(_.executeWithNoFailureFunction)
  }
}
