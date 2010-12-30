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

/**
 * This object provides useful control abstractions
 */
object Control {
  /**
   * save the value of a variable, set it temporarily to another value and reset it when f is executed
   */
  def setTemporarily[T, U](current: T, temp: T, setter: T => Unit)(f: =>U) = {
    var saved = current
    try {
      setter(temp)
      f
    } finally {
      setter(saved)
    }
  }
  /**
   * save the value of 2 variables, set them temporarily to other values and reset them when f is executed
   */
  def setTemporarily[S, T, U](current1: S, temp1: S, setter1: S => Unit, 
                              current2: T, temp2: T, setter2: T => Unit)(f: =>U): U = {
    setTemporarily(current1, temp1, setter1) {
      setTemporarily(current2, temp2, setter2) {
        f
      }      
    }
  }
  /**
   * save the value of 3 variables, set them temporarily to other values and reset them when f is executed
   */
  def setTemporarily[R, S, T, U](current1: R, temp1: R, setter1: R => Unit, 
                                 current2: S, temp2: S, setter2: S => Unit,
                                 current3: T, temp3: T, setter3: T => Unit)(f: =>U): U = {
    setTemporarily(current1, temp1, setter1) {
      setTemporarily(current2, temp2, setter2) {
        setTemporarily(current3, temp3, setter3) {
          f
        }
      }      
    }
  }
}
