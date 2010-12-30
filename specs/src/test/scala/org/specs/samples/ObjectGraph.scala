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
package org.specs.samples
import org.specs.runner._
import org.specs.matcher._
import org.specs.specification._
import org.specs._

class objectGraphSpec extends SpecificationWithJUnit with ObjectGraphMatchers {
  "The following object graphs" should {
    "match when they are the same" in {
      val foo = new Foo("2")
      foo.bars += new Bar(33)
      foo.bars += new Bar(12)

      val foo2 = new Foo("2")
      foo2.bars += new Bar(12)
      foo2.bars += new Bar(33)

      foo must matchFoo(foo2)
    }
  }
}

trait ObjectGraph {
  import scala.collection.mutable

  case class Foo(val name:String) {
    var solobar: Bar = null
    val bars = mutable.Set[Bar]()
  }
  case class Bar(val id: Long)
}

trait ObjectGraphMatchers extends ObjectGraph with Matchers {
  case class matchFoo(foo: Foo) extends Matcher[Foo] {
    def apply(other: => Foo) = {
      ((beEqualTo(_:String)) ^^^ ((_:Foo).name) and
       (matchOptionalBar(_)) ^^^ ((_:Foo).solobar) and
       (matchBar(_)).toSet ^^^ ((_:Foo).bars))(foo)(other)
    }
  }
  case class matchOptionalBar(bar: Bar) extends Matcher[Bar] {
    def apply(other: => Bar) = {
      ((beAsNullAs(_:Bar)) or
      ((matchBar(_))))(bar)(other)
    }
  }
  case class matchBar(bar: Bar) extends Matcher[Bar] {
    def apply(other: => Bar) = {
      ((beEqualTo(_:Long)) ^^^ ((_: Bar).id))(bar)(other)
    }
  }
}
