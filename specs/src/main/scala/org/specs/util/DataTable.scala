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
package org.specs.util
import Products._
import scala._
import org.specs.specification._
import scala.xml.NodeSeq
import org.specs.execute._
/**
 * The Datatables trait provides implicit methods to start table headers
 * and DataRows.<p>
 */
trait DataTables {
  /**
   * @return a table header which first column is the string <code>a</code>
   */
  implicit def toTableHeader(a: String) = new TableHeader(List(a))
  /**
   * @return a table header with a context
   */
  implicit def toTableHeaderWithContext(c: Context) = new TableHeader(c)

  /**
   * @return a table row whose type will be <code>T</code> for each element and
   * which starts with <code>a</code> as the first element
   */
  implicit def toDataRow[T](a: T) = DataRow1(a)
}

/**
 * The TableHeader case class models the header of a data table which should be a list of strings.<p>
 * A header can be created using the | operator a separator between strings<pre>
 * "a" | "b" | "c = a + b"|
 * </pre>
 * A header can be closed using the | method which will return the TableHeader object<p>
 * A header can be followed by data rows which only requirement is to have a <code>def header_=(t: TableHeader)</code> function
 */
case class TableHeader(var titles: List[String], var isOk: Boolean, var context: Option[Context]) {
  /** Default constructor. */
  def this(titles: List[String]) = this(titles, true, None)
  /** constructor with a context. */
  def this(c: Context) = this(Nil, true, Some(c))
  /** Mutator to indicate a failure in the rest of the table. */
  def setFailed() = isOk = false
  /**
   * Adds a new column to the header
   * @returns the extended header
   */
  def |(s: String) = { titles = titles ::: List(s); this }

  /**
   * Used to close the header
   * @returns the header
   */
  def | = this

  /**
   * Used to close the header and set a context
   * @returns the header
   */
  def |(c: Context) = {
    context = Some(c)
    this
  }

  /**
   * Accepts any object on which a header can be set. Sets the header on that object and returns it
   * @returns the header-accepting object (usually a DataRow object)
   */
  def |[T <: ExecutableHeader](d: T) = { d.header_=(this); d }

  /**
   * Accepts any object on which a header can be set and which is executable.
   * Sets the header on the object, marks it as "should be executed" and returns it.
   * @returns the header-accepting object (usually a DataRow object), ready to execute
   */
  def |>[T <: ExecutableHeader](d: T) = { d.header_=(this); d.shouldExecute_=(true); d }

  /**
   * @returns the header as string: |"a" | "b" | "c = a + b"|
   */
   override def toString = titles.mkString("|", "|", "|")
  /**
   * @returns the header as html
   */
   def toXhtml = {
     <tr>{
       titles.map((t:Any) => <th>{t.toString}</th>)}{
       if (!isOk) <th><img src="images/icon_failure_sml.gif"/></th> else NodeSeq.Empty
     }</tr>
   }
   /** 
    * store a reference to the following table. 
    * This is a convenient way to grab the whole table from the header when the implicit creating the header
    * stores the header in a local variable. @see org.specs.form.DataTableForm for the use of this feature.
    */
   var table: Option[ExecutableDataTable] = None
   def setTable(table: ExecutableDataTable) = { this.table = Some(table); this }
   /** execute the table with no failure function */
   def executeWithNoFailureFunction = executeWithFailureFunction((t: ExecutableDataTable) => ())
   /** execute the table with a specific failure function */
   def executeWithFailureFunction(f: ExecutableDataTable => Unit) = {
     table map { (t:ExecutableDataTable) =>
       t.whenFailing(f).execute
     }
     this
   }
   /** execute the table */
   def execute = {
     table.map(_.execute)
     this
   }
}
/**
 * Helper trait to support the passing of a header from row to row
 */
private[util] trait ExecutableHeader {
  def header_=(t: TableHeader)
  def shouldExecute_=(b: Boolean)
}
/**
 * Abstract representation of a row without the column types.
 */
trait AbstractDataRow extends DefaultResults with ExecutableHeader {
  var header: TableHeader = new TableHeader(Nil, true, None)
  var shouldExecute = false;
  def valuesList: List[Any]
}
abstract class DataRow[+T0, +T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10,
                       +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19](val values: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)) extends AbstractDataRow {
  def | = this
  def |[S0 >: T0, S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7, S8 >: T8, S9 >: T9,
        S10 >: T10, S11 >: T11, S12 >: T12, S13 >: T13, S14 >: T14,
        S15 >: T15, S16 >: T16, S17 >: T17, S18 >: T18, S19 >: T19](row: DataRow[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19]): DataTable[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19] = {
    setFunction(row, makeTable(row, shouldExecute), false)
  }
  /** produced with:
   * def types(n: Int) = (0 to n).toList.map("S" + _.toString).mkString(", ")
   * def values(n: Int) = (1 to n+1).map("values._" + _.toString).mkString(", ")
   * def template(n: Int) = (0 to n).map(i => <t>case DataRow1(f: Function{i+1}[{types(i)}, _]) => table.function = () => {{ f({values(i)}); table }}</t>.text).mkString("\n")
   * println(template(19))
   */
  private def setFunction[S0 >: T0, S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7, S8 >: T8, S9 >: T9,
        S10 >: T10, S11 >: T11, S12 >: T12, S13 >: T13, S14 >: T14,
        S15 >: T15, S16 >: T16, S17 >: T17, S18 >: T18, S19 >: T19](row: DataRow[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19], 
                                                                    table: DataTable[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19],
                                                                      execute: Boolean) = {
    row match {
      case DataRow1(f: Function1[_, _]) => table.function = () => { f.asInstanceOf[Function1[S0, _]](values._1); table }; if (execute) table.execute
      case DataRow1(f: Function2[_, _, _]) => table.function = () => { f.asInstanceOf[Function2[S0, S1, _]](values._1, values._2); table }; if (execute) table.execute
      case DataRow1(f: Function3[_, _, _, _]) => table.function = () => { f.asInstanceOf[Function3[S0, S1, S2, _]](values._1, values._2, values._3); table }; if (execute) table.execute
      case DataRow1(f: Function4[_, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function4[S0, S1, S2, S3, _]](values._1, values._2, values._3, values._4); table }; if (execute) table.execute
      case DataRow1(f: Function5[_, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function5[S0, S1, S2, S3, S4, _]](values._1, values._2, values._3, values._4, values._5); table }; if (execute) table.execute
      case DataRow1(f: Function6[_, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function6[S0, S1, S2, S3, S4, S5, _]](values._1, values._2, values._3, values._4, values._5, values._6); table }; if (execute) table.execute
      case DataRow1(f: Function7[_, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function7[S0, S1, S2, S3, S4, S5, S6, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7); table }; if (execute) table.execute
      case DataRow1(f: Function8[_, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function8[S0, S1, S2, S3, S4, S5, S6, S7, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8); table }; if (execute) table.execute
      case DataRow1(f: Function9[_, _, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function9[S0, S1, S2, S3, S4, S5, S6, S7, S8, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8, values._9); table }; if (execute) table.execute
      case DataRow1(f: Function10[_, _, _, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function10[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8, values._9, values._10); table }; if (execute) table.execute
      case DataRow1(f: Function11[_, _, _, _, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function11[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8, values._9, values._10, values._11); table }; if (execute) table.execute
      case DataRow1(f: Function12[_, _, _, _, _, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function12[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8, values._9, values._10, values._11, values._12); table }; if (execute) table.execute
      case DataRow1(f: Function13[_, _, _, _, _, _, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function13[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8, values._9, values._10, values._11, values._12, values._13); table }; if (execute) table.execute
      case DataRow1(f: Function14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function14[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8, values._9, values._10, values._11, values._12, values._13, values._14); table }; if (execute) table.execute
      case DataRow1(f: Function15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function15[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8, values._9, values._10, values._11, values._12, values._13, values._14, values._15); table }; if (execute) table.execute
      case DataRow1(f: Function16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function16[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8, values._9, values._10, values._11, values._12, values._13, values._14, values._15, values._16); table }; if (execute) table.execute
      case DataRow1(f: Function17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function17[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8, values._9, values._10, values._11, values._12, values._13, values._14, values._15, values._16, values._17); table }; if (execute) table.execute
      case DataRow1(f: Function18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function18[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8, values._9, values._10, values._11, values._12, values._13, values._14, values._15, values._16, values._17, values._18); table }; if (execute) table.execute
      case DataRow1(f: Function19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function19[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8, values._9, values._10, values._11, values._12, values._13, values._14, values._15, values._16, values._17, values._18, values._19); table }; if (execute) table.execute
      case DataRow1(f: Function20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]) => table.function = () => { f.asInstanceOf[Function20[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19, _]](values._1, values._2, values._3, values._4, values._5, values._6, values._7, values._8, values._9, values._10, values._11, values._12, values._13, values._14, values._15, values._16, values._17, values._18, values._19, values._20); table }; if (execute) table.execute
      case _ => if (!execute) table.rows = List(this, {row.header_=(header); row}); table
    }
    table
  }
  private def makeTable[S0 >: T0, S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7, S8 >: T8, S9 >: T9,
        S10 >: T10, S11 >: T11, S12 >: T12, S13 >: T13, S14 >: T14,
        S15 >: T15, S16 >: T16, S17 >: T17, S18 >: T18, S19 >: T19](row: DataRow[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19], execute: Boolean) = {
    val table = DataTable(header, List(this, {row.header_=(header); row}), execute)
    table.rows = List(this)
    setFunction(row, table, execute)
    header.setTable(table)
    table
  }
  
  def |>[S0 >: T0, S1 >: T1, S2 >: T2, S3 >: T3, S4 >: T4, S5 >: T5, S6 >: T6, S7 >: T7, S8 >: T8, S9 >: T9,
        S10 >: T10, S11 >: T11, S12 >: T12, S13 >: T13, S14 >: T14,
        S15 >: T15, S16 >: T16, S17 >: T17, S18 >: T18, S19 >: T19](row: DataRow[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19]): DataTable[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19] = {
    setFunction(row, makeTable(row, true), true)
  }
  def valuesList = {
    var l: List[Any] = Nil
    for (i <- new Range(0, values.productArity, 1);
         e <- values.productElement(i) if (e != None))
           l = l:::List(e)
    l
  }
  override def toString = valuesList.mkString("|", "|", "|")
  def result = statusAsText + toString + (if (isOk) "" else " ") + issues.map(_.getMessage).mkString(",")
  def toXhtml = <tr class={statusClass}>
    { valuesList.map((v:Any) => <td>{v.toString}</td>) }
    { if (header.isOk) NodeSeq.Empty else <td>{issues.map(_.getMessage)}</td> }
    </tr>
  def toHtml = new scala.xml.PrettyPrinter(100000, 2).format(toXhtml)
}
trait ExecutableDataTable extends HasResults {
  def execute: this.type
  def results: String
  def rows: List[AbstractDataRow]
  def toHtml = new scala.xml.PrettyPrinter(100000, 2).format(scala.xml.Group(toXhtml))
  def toXhtml: NodeSeq
  def failures: Seq[FailureException] = rows.flatMap(_.failures)
  def errors: Seq[Throwable] = rows.flatMap(_.errors)
  def skipped: Seq[SkippedException] = rows.flatMap(_.skipped)
  def reset() = { rows.foreach { r => r.reset() }}
  def whenFailing(f: ExecutableDataTable => Unit): this.type
  var context : Option[Context] = None
  def contextIs(c: Context) = {
    context = Some(c)
    this
  }
}

/**
 * A DataTable contains: a header describing the columns, datarows containing values, a function to execute on each row
 * In the following example of a DataTable:<code>
 * val datatable = "a" | "b" | "c = a + b" |>
 *                  1  !  2  !     3       |
 *                  2  !  2  !     4       |
 *                  3  !  2  !     5       | {
 *                (a: Int, b: Int, c: Int) => c must_== calc.add(a, b)
 *                 } </code>
 * The header defines 3 columns, there are 3 rows of type (Int, Int, Int) and a function taking its values in each row.
 * The '>' sign added to the header means that the function will be executed on each row when the table is defined. The result
 * of the execution will be available via a <code>results</code> function returning a string.<p>
 * A DataTable has the following constraints:<ul>
 * <li/>It cannot have more than 20 columns
 * <li/>The rows must be of identical types
 * <li/>The function must have the same parameter types as the types of the rows (However the function can have less parameters than the number of row cells)
 * </ul>
 */
case class DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](header: TableHeader, var rows: List[DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]], var shouldExecute: Boolean) extends ExecutableDataTable { outer =>

  type AbstractDataRow = DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
  /**
   * This function can be overriden to provide another behaviour upon table failure
   */
  def failureFunction(table: ExecutableDataTable) : Unit = {
    header.setFailed()
    tableFailureFunction(table)
  }
  /**
   * This sets a failing function to use when the table fails
   */
  def whenFailing(f: ExecutableDataTable => Unit) = {
    tableFailureFunction = f
    this
  }
  protected var tableFailureFunction: ExecutableDataTable => Unit = { t => throw new DataTableFailureException(this) }

  /**
   * function to execute on each row
   */
  var function : Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = _

  /**
   * Datatable constructor with an empty header
   */
  def this(rows: List[DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]) = this(new TableHeader(Nil), rows, false)

  /**
   * Adds a new datarow to the existing table
   */
  def |(r: DataRow[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]) = {
    val table = DataTable(header, rows:::List({r.header_=(header); r}), shouldExecute)
    header.setTable(table)
    table
  }

  /**
   * Adds a new datarow to the existing table and sets the table for immediate execution
   */
  def |>(r: AbstractDataRow) = { this.|(r); shouldExecute = true; this }

  /**
   * closes the table
   */
  def | = this

  /**
   * executes the function on each table row if the function exists
   */
  def execute: this.type = execute { 
    if (function != null) { 
      function() 
    } 
  }
  def execute(f: =>Any): this.type = {
    reset()
    f
    if (!isOk) failureFunction(this)
    this
  
  }
  /**
   * @returns the result of the execution of the table
   */
  def results: String = {
    val result =
     header.toString + "\n" +
     rows.map(_.result).mkString("\n")

    def alignRows = " " + result.replaceAll("\n\\|", "\n \\|")
    alignRows
  }

  /**
   * @returns a string representation of the table
   */
  override def toString = (header.toString + "\n" + rows.mkString("\n"))
  /**
   * @returns an html representation of the table
   */
  def toXhtml = <table class="dataTable">{header.toXhtml}{rows map(_.toXhtml)}</table>

  /**
   * execute a row by checking the execution of the user-supplied function and
   * either displaying the row or displaying the row and an error message
   */
  private def executeRow(row: AbstractDataRow, value: => Any) = {
    try {
      header.context.map(_.beforeActions())
      header.context match {
        case Some(c) => c.aroundExpectationsActions(value)
        case None => value
      }
    }
    catch {
      case f: FailureException => row.addFailure(f)
      case s: SkippedException => row.addSkipped(s)
      case e: Throwable => { e.printStackTrace; row.addError(e) }
    } finally {
      header.context.map(_.afterActions())
    } 
  }
  /**
   * apply a function of one argument to the table and set the table for execution
   */
  def |>[R](f: Function1[T0, R]) = {shouldExecute = true; this.|(f)}

  /**
   * apply a function of one argument to the table
   */
  def |[R](f: Function1[T0, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function2[T0, T1, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function2[T0, T1, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function3[T0, T1, T2, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function3[T0, T1, T2, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function4[T0, T1, T2, T3, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function4[T0, T1, T2, T3, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function5[T0, T1, T2, T3, T4, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function5[T0, T1, T2, T3, T4, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function6[T0, T1, T2, T3, T4, T5, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function6[T0, T1, T2, T3, T4, T5, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function7[T0, T1, T2, T3, T4, T5, T6, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function7[T0, T1, T2, T3, T4, T5, T6, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function8[T0, T1, T2, T3, T4, T5, T6, T7, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function8[T0, T1, T2, T3, T4, T5, T6, T7, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function9[T0, T1, T2, T3, T4, T5, T6, T7, T8, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function9[T0, T1, T2, T3, T4, T5, T6, T7, T8, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17, r.values._18))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17, r.values._18, r.values._19))}; outer }    }
    if (shouldExecute) execute else this
  }
  def |>[R](f: Function20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]) = {shouldExecute = true; this.|(f)}
  def |[R](f: Function20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]) = {

    function = new Function0[DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]() {       def apply(): DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {rows foreach {r => executeRow(r, f(r.values._1, r.values._2, r.values._3, r.values._4, r.values._5, r.values._6, r.values._7, r.values._8, r.values._9, r.values._10, r.values._11, r.values._12, r.values._13, r.values._14, r.values._15, r.values._16, r.values._17, r.values._18, r.values._19, r.values._20))}; outer }    }
    if (shouldExecute) execute else this
  }
}
case class DataRow1[T0](v0: T0) extends DataRow((v0, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow2[T0, T](v0, v)
}
case class DataRow2[T0, T1](v0: T0, v1: T1) extends DataRow((v0, v1, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow3[T0, T1, T](v0, v1, v)
}
case class DataRow3[T0, T1, T2](v0: T0, v1: T1, v2: T2) extends DataRow((v0, v1, v2, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow4[T0, T1, T2, T](v0, v1, v2, v)
}
case class DataRow4[T0, T1, T2, T3](v0: T0, v1: T1, v2: T2, v3: T3) extends DataRow((v0, v1, v2, v3, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow5[T0, T1, T2, T3, T](v0, v1, v2, v3, v)
}
case class DataRow5[T0, T1, T2, T3, T4](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4) extends DataRow((v0, v1, v2, v3, v4, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow6[T0, T1, T2, T3, T4, T](v0, v1, v2, v3, v4, v)
}
case class DataRow6[T0, T1, T2, T3, T4, T5](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5) extends DataRow((v0, v1, v2, v3, v4, v5, None, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow7[T0, T1, T2, T3, T4, T5, T](v0, v1, v2, v3, v4, v5, v)
}
case class DataRow7[T0, T1, T2, T3, T4, T5, T6](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6) extends DataRow((v0, v1, v2, v3, v4, v5, v6, None, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow8[T0, T1, T2, T3, T4, T5, T6, T](v0, v1, v2, v3, v4, v5, v6, v)
}
case class DataRow8[T0, T1, T2, T3, T4, T5, T6, T7](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, None, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow9[T0, T1, T2, T3, T4, T5, T6, T7, T](v0, v1, v2, v3, v4, v5, v6, v7, v)
}
case class DataRow9[T0, T1, T2, T3, T4, T5, T6, T7, T8](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, None, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v)
}
case class DataRow10[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, None, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v)
}
case class DataRow11[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, None, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v)
}
case class DataRow12[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, None, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v)
}
case class DataRow13[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, None, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v)
}
case class DataRow14[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, None, None, None, None, None, None)) {
  def ![T](v: T) = DataRow15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v)
}
case class DataRow15[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, None, None, None, None, None)) {
  def ![T](v: T) = DataRow16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v)
}
case class DataRow16[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, None, None, None, None)) {
  def ![T](v: T) = DataRow17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v)
}
case class DataRow17[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, None, None, None)) {
  def ![T](v: T) = DataRow18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v)
}
case class DataRow18[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, None, None)) {
  def ![T](v: T) = DataRow19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v)
}
case class DataRow19[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, None)) {
  def ![T](v: T) = DataRow20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v)
}
case class DataRow20[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19) extends DataRow((v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19)) {

}
/**
 * Extension of a FailureException to allow a better display of this kind of failure (used in HtmlRunner)
 */
class DataTableFailureException(val table: ExecutableDataTable) extends FailureException(table.results)
