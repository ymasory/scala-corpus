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
import org.specs._
import org.specs.specification._
import org.specs.runner._
import org.specs.execute._

class dataTableUnit extends SpecificationWithJUnit with DataTables {
  "a data table" should {
    "be just a datarow if it has one single row" in {
      val data = "a"|"b"|"c"|
                  1 ! 2 ! 3 |

      data.getClass.getName must beMatching("DataRow")
    }
    "be just a datatable if appended a function" in {
      var total = 0
      val data = "a" | "b" |
                  1  ! 2   |> { (a: Int, b: Int) => 
                    total = a + b
                  }
      data.getClass.getName must beMatching("DataTable")
      data.header.toString aka "the table header" must_== "|a|b|"
      data.rows.size aka "the number of rows" must_== 1
      total must_== 3
    }
    "be a datatable if it has at least 2 rows" in {
      val data = "a"|"b"|"c"|
                  1 ! 2 ! 3 |
                  1 ! 2 ! 3 |> {(a: Int, b: Int, c: Int) => ()}

      data.getClass.getName must beMatching("DataTable")
    }
    "have a toString method printing out all its rows, in the user-defined order, separated by a new line" in {
      val datatable = "a"|"b"|"c"|
                       1 ! 0 ! 1 |
                       1 ! 1 ! 2 |
                       1 ! 2 ! 3 |> {(a: Int, b: Int, c: Int) => ()}

      datatable.toString must_== "|a|b|c|\n" +
                                 "|1|0|1|\n" +
                                 "|1|1|2|\n" +
                                 "|1|2|3|"
    }
    "provide the results of the execution of a function over all rows" in {
      val datatable = "a"|"b"|"c"|
                       1 ! 2 ! 3 |
                       1 ! 2 ! 3 |> {(a: Int, b: Int, c: Int) => true}

      datatable.results must_== " |a|b|c|\n" +
                                "+|1|2|3|\n" +
                                "+|1|2|3|"
    }
    "fail if at least on row fails the function" in {
       val datatable =  "a"|"b"|"c"|
                         1 ! 2 ! 3 |
                         1 ! 2 ! 3 |{(a: Int, b: Int, c: Int) => throw FailureException("fail")}
       try { datatable.execute } catch { case _ => true }

      datatable.isOk must beFalse
    }
    "provide the results of the execution of a function over all rows, showing failures if the function throws an exception" in {
       val datatable =  "a"|"b"|"c"|
                         1 ! 2 ! 4 |
                         1 ! 2 ! 3 |{(a, b, c) => a+b must_== c}
       try { datatable.execute }
       catch { case _ => true }

      datatable.results must_== " |a|b|c|\n" +
                                "x|1|2|4| '3' is not equal to '4'\n" +
                                "+|1|2|3|"
    }
    "allow type inference on cell types" in {
      val row1 = true ! true
      val row2 = 1    ! true
      val t = row1 | row2
      t.isExpectation
    }
    "have a whenFailing method to replace the failing method called when the table fails" in {
       var theNewFunctionIsCalled = false
       val datatable =  "a"|"b"|"c"|
                         1 ! 2 ! 4 |
                         1 ! 2 ! 3 |

       datatable.whenFailing { t =>  theNewFunctionIsCalled = true } |> { (a:Int,b:Int,c:Int) => a + b  must_== c }
       theNewFunctionIsCalled must beTrue
    }
    "store the result of each row when executed" in {
       val datatable =  "a"|"b"|"c"|
                         1 ! 2 ! 4 |
                         1 ! 2 ! 3 |

       datatable.whenFailing { t => () } |> { (a:Int,b:Int,c:Int) => a + b  must_== c }
       datatable.rows must have((r: AbstractDataRow) => !r.isOk)
    }
    "show the status of the row when using the toXhtml method" in {
       val datatable =  "a"|"b"|"c"|
                         1 ! 2 ! 4 |
                         1 ! 2 ! 3 |

       datatable.whenFailing { t => () } |> { (a,b,c) => a + b  must_== c }
       datatable.rows(0).toXhtml must \\("tr", Map("class"->"failure"))
    }
    "have a toXhtml method displaying the rows in an html table" in {
       val datatable = "a"|"b"|"c"|
                        1 ! 2 ! 3 |
                        1 ! 2 ! 3 |
                        1 ! 2 ! 3 |

       datatable.whenFailing { t => () } |>  { (a,b,c) => a + b  must_== c }

       datatable.toXhtml must beEqualToIgnoringSpace(<table class="dataTable">
         <tr><th>a</th><th>b</th><th>c</th></tr>
         <tr class="success"><td>1</td><td>2</td><td>3</td></tr>
         <tr class="success"><td>1</td><td>2</td><td>3</td></tr>
         <tr class="success"><td>1</td><td>2</td><td>3</td></tr>
       </table>)
    }
    "have a toXhtml method showing the failure messages if any" in {
       val datatable = "a"|"b"|"c"|
                        1 ! 2 ! 3 |
                        1 ! 2 ! 4 |
                        2 ! 2 ! 4 |

       datatable.whenFailing { t => () } |>  { (a,b,c) => a + b  must_== c }

       datatable.toXhtml must beEqualToIgnoringSpace(<table class="dataTable">
         <tr><th>a</th><th>b</th><th>c</th><th><img src="images/icon_failure_sml.gif"/></th></tr>
         <tr class="success"><td>1</td><td>2</td><td>3</td><td/></tr>
         <tr class="failure"><td>1</td><td>2</td><td>4</td><td>'3' is not equal to '4'</td></tr>
         <tr class="success"><td>2</td><td>2</td><td>4</td><td/></tr>
       </table>)
    }
    "allow a context to be used around rows" in {
       var v = 1
       beforeContext(v = 0)|
       "a"|"b"|"c"|  
        2 ! 2 ! 4 | 
        1 ! 2 ! 3 |> { (a, b, c) =>
         v must_== 0
         v = 1
        } 
    }
  }
  "A datatable" can {
    "have a context associated to its header" in {
      val h = "a"|"b"|"c"|(beforeContext())
      h.context must beSome
      (h|1!2!3).header.context must beSome
    }
    "use the associated context when executing the table" in {
      var d = 1
      "a"|"b"|"c"| beforeContext(d = 0) |
       1 ! 2 ! 3 |
       2 ! 2 ! 4 |> { (a, b, c) => 
         d must_== 0
         (a+b) must_== c
       }
    }
  }
}
