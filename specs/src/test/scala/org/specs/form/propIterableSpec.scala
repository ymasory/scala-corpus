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

class propIterableSpec extends org.specs.SpecificationWithJUnit {
  "An iterable Prop toString function" should {
    "display iterable values" in {
      PropIterable("label", List(1.234, 2.456)).toString must_== "label: 1.234, 2.456 (expected: _)"
    }
  }
  "An iterable Prop" should {
    "be able to change its value formatter" in {
      val p = PropIterable[Int]("label", List(1, 2))
      p.formatterIs((i:Int) => "v: "+i.toString)
      p.formattedValue.toString must_== "v: 1, v: 2"
    }
    "be able to change its value formatter" in {
      val p = PropIterable(List(1))
      p.formatterIs((i:Int) => "v: "+i.toString)
      p.formattedValue.toString must_== "v: 1"
    }
  }
  "An iterable property toXhtml method" should {
    val l = List(1.234, 2.345)
    "display one value only if there is only one" in {
      PropIterable("", List(1.234)).toXhtml must ==/(<td class="info">1.234</td>)
    }
    "display comma-separated values if there are more than one" in {
      PropIterable("", l).toXhtml must ==/(<td class="info">1.234, 2.345</td>)
    }
    "display values with another separator if the valuesFormatter is changed" in {
      val p = PropIterable("", l)
      p.formatWith((l:Option[Iterable[Double]]) => l match { 
        case None => ""
        case Some(x) => x.map(p.formatValue(_)).mkString("/")
      })
      p.toXhtml must ==/(<td class="info">1.234/2.345</td>)
    }
    "display values with another formatter if the valueFormatter is changed" in {
      val p = PropIterable("", l)
      p.formatValueWith((l:Option[Double]) => l match { 
        case None => "0.0"
        case Some(d) => new java.text.DecimalFormat("#.#").format(d)
      })
      p.toXhtml must ==/(<td class="info">1.2, 2.3</td>)
    }
  }
}
