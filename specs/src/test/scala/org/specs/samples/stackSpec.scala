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
import org.scalacheck.Commands
import org.specs.util._
import org.specs._

class stackSpec extends StackSpecification {
  var stack = emptyStack 
  val nonEmptyStack = beforeContext(stack = nonEmpty)
  val belowCapacityStack = beforeContext(stack = belowCapacity)
  val fullStack = beforeContext(stack = full)
  
  "An empty stack" should {
    "throw an exception when sent #top" in { 
      stack.top must throwA[NoSuchElementException]
    }
    "throw an exception when sent #pop" in { 
      stack.pop must throwA[NoSuchElementException]
    }
  }
  "A non-empty stack below full capacity" ->-(nonEmptyStack) should {
    "not be empty" in { 
      stack verifies (!_.isEmpty)
    }
    "return the top item when sent #top" in { 
      stack.top mustBe stack.lastItemAdded
    }
    "not remove the top item when sent #top" in { 
      stack.top mustBe stack.lastItemAdded
      stack.top mustBe stack.lastItemAdded
    }
    "return the top item when sent #pop" in { 
      stack.pop mustBe stack.lastItemAdded
    }
    "remove the top item when sent #pop" in { 
      stack.pop mustBe stack.lastItemAdded
      if (!stack.isEmpty)
        stack.top mustNotBe stack.lastItemAdded
    }
  }
  "A stack below full capacity"->-(belowCapacityStack) should {
    behave like "A non-empty stack below full capacity"
    "add to the top when sent #push" in { 
      stack push 3
      stack.top mustBe 3
    }
  }
  "A full stack"->-(fullStack) should {
    behave like "A non-empty stack below full capacity"
    "throw an exception when sent #push" in {
      stack.push(11) must throwAn[Error]
    }
  }
}

class StackSpecification extends SpecificationWithJUnit {
  case class SampleStack(name: String, stackCapacity: Int, itemsNb: Int) extends LimitedStack[Int](stackCapacity) {
    def this(name: String, capacity: Int) = this(name, capacity, 0)
    var lastItemAdded = 0
    for (i <- 1 to itemsNb) { super.push(i); lastItemAdded = i }
    override def toString = name
  }
  def emptyStack = new SampleStack("empty stack", 10)
  def full = new SampleStack("full stack", 10, 10)
  def nonEmpty = new SampleStack("non empty stack", 10, 2)
  def belowCapacity = new SampleStack("below capacity stack", 10, 3)
}

class LimitedStack[T](val capacity: Int) extends scala.collection.mutable.Stack[T] {
  override def push(a: T) = {
    if (size >= capacity) throw new Error("full stack") else super.push(a)
  }
}