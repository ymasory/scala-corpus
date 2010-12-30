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

class dataRowUnit extends SpecificationWithJUnit with DataTables {
  type DR = DataRow3[Int, Int, Int]
  "a data row" should {
    val datarow = DataRow3[Int, Int, Int](1, 2, 3)
    "print out its values separated by |" in { 
      datarow.toString must_== "|1|2|3|"
    }
    "have a toHtml method setting success if the row hasn't failed" in { 
      datarow.toXhtml must \\("tr", Map("class"->"success"))
    }
    "have a toHtml method setting failure if the row has failed" in { 
      datarow.addFailure(new FailureException("failed"))
      datarow.toXhtml must \\("tr", Map("class"->"failure"))
    }
    "have a toHtml method setting error if the row has an error" in { 
      datarow.addError(new java.lang.Error("error"))
      datarow.toXhtml must \\("tr", Map("class"->"error"))
    }
  }
  "a data row" should {
    "have the same header as its previous row" in {
      val datarow = 1!2!3|

      datarow.header = "a"|"b"|"c"
      (datarow | 4!5!6).header must be(datarow.header)
    }
    "be able to access its header status" in {
      val table = "a"|"b"|"c"|
                   1!2!3|
                   4!5!6|
                   7!8!9
      table.header.setFailed()
      table.rows(0).header.isOk must beFalse
      table.rows(1).header.isOk must beFalse
      table.rows(2).header.isOk must beFalse
    }

  }
}
