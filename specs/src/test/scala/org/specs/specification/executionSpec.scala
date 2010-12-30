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
package org.specs.specification

class executionSpec extends org.spex.Specification {
  include(new execution0, new execution1, new execution2, new execution3)
}
class execution0 extends org.spex.Specification {
  var x = 1
  "A sus" should {
    "execute the first example with local variables" in { x must_== 1; x = x + 1 }
    "execute the second example with a reinitialized variable" in { x must_== 1; x = x + 1 }
  }
}
class execution1 extends org.spex.Specification {
  var x = 1
  "A sus" should {
    x = x + 1
    "execute the first example with local variables" in { 
      x aka "ex1" must_== 2; 
      x = x + 1 }
    "execute the second example with a reinitialized variable" in { 
      x aka "ex2" must_== 2; 
      x = x + 1 }  
  }
  "Another sus " should {
    x = x + 1
    "2 - execute the first example with local variables" in { x must_== 2; x = x + 1 }
    "2 - execute the second example with a reinitialized variable" in { x must_== 2; x = x + 1 }
  }
}
class execution2 extends org.spex.Specification {
  "A sus" should {
    var x = 1
    def inc() = {
      x = x + 1
    }
    "execute the first example with local variables" in { 
      inc()
      "use the local variable in the first subexample" in { x must_== 2 }
      "reinit the local variable for the second subexample" in { x must_== 2 }
    }
  }
}
class execution3 extends org.spex.Specification {
  "A sus" should {
    var x = 1
    "execute the first example with local variables" in { 
      x = x + 1
      "use the local variable in the first subexample" in { 
        x = x + 1
        "use the local variable in the sub-subexample" in { 
          x must_== 3 
        } 
      }
      
    }
  }
}
class execution4 extends org.spex.Specification {
  var x = 1
  "A sus" should {
    "execute the first example with local variables" in {  
      x = x + 1
      "use the local variable in the first subexample" in { 
        x = x + 1
        x must_== 3 
      }
    }
  }
  "Another sus" should {
    "execute the first example with local variables - 2" in {  
      x = x + 1
      "use the local variable in the first subexample - 2" in { 
        x = x + 1
        x must_== 3 
      }
    }
  }
}
class execution5 extends org.spex.Specification {
  var desc = "2 systems with the same name but different examples"
  var x = 1
  "A sus" should {
    "execute the first example" in { x must_== 1; x += 1 }
  }
  "A sus" should {
    "execute this other example" in { x must_== 1 }
  }
}