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
package org.specs.matcher

class xUnitSpec extends MatchersSpecification with xUnit {
  "the xUnit trait" should {
    "provide an assertTrue method" in {
      assertTrue(1 == 1)
      expectation { assertTrue(1 == 2) } must failWith("the value is false")
    }
    "provide an assertFalse method" in {
      assertFalse(1 == 2)
      expectation { assertFalse(1 == 1) } must failWith("the value is true")
    }
    "provide an assertEquals method" in {
      assertEquals(1, 1)
      assertEquals("s", "s")
      expectation { assertEquals(1, 2) } must failWith("'1' is not equal to '2'")
    }
    "provide an assertSame method" in {
      val s = "s" * 2
      assertSame(s, s)
      expectation { assertSame(s, "ss") } must failWith("'ss' is not the same as 'ss'")
    }
    "provide an assertNotSame method" in {
      val s = "s" * 2
      assertNotSame(s, "ss")
      expectation { assertNotSame(s, s) } must failWith("'ss' is the same as 'ss'")
    }
    "provide an assertNull method" in {
      assertNull(null)
      expectation { assertNull("1") } must failWith("'1' is not null")
    }
    "provide an assertNotNull method" in {
      assertNotNull(1)
      expectation { assertNotNull(null) } must failWith("the value is null")
    }
    "provide an assertArrayEquals method" in {
      assertArrayEquals(Array(1, 2, 3), Array(1, 2, 3))
      expectation { assertArrayEquals(Array(1, 2, 3), Array(1, 5, 6)) } must failWith("'2' is not equal to '5'; '3' is not equal to '6'")
    }
  }
}
