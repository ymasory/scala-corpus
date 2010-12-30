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
import org.specs.util.{ Configuration }
import org.specs.util.Classes._
/**
 * This trait executes an example by cloning the enclosing specification first.
 * 
 * This way, the example is executed in a total isolation so as not to share local variables between examples
 * and avoid side-effects.
 * 
 * Warning: this works by considering that the "examples" method is stable on a BaseSpecification and
 * will always return the same examples in the same order
 */
trait SpecificationExecutor extends LifeCycle { this: BaseSpecification with ExampleExpectationsListener =>
  /** execute an example by cloning the specification and executing the cloned example */
  override def executeExample(example: Examples): this.type = {
    super.executeExample(example)
    var executed = false
    try {
      val path  = example.pathFromRoot
      if (oneSpecInstancePerExample && !executeOneExampleOnly && !path.isFirst) {
        cloneSpecification match {
          case None => example.executeThis
          case Some(s) => {
            s.executeOneExampleOnly = true
            s.expectationsListener = this
            s.beforeSpecFailure = this.beforeSpecFailure
            s.beforeSpecHasBeenExecuted = this.beforeSpecHasBeenExecuted
            s.afterSpecFailure = this.afterSpecFailure
            s.afterSpecHasBeenExecuted = this.afterSpecHasBeenExecuted
            s.parent = Some(this)
            val cloned = s.getExample(path)
            cloned match {
              case None => throw PathException(path + "not found for " + example)
              case Some(c) => {
                c.prepareExecutionContextFrom(example)
                c.execution.map(_.example = c)
                c.execution.map(_.execute)
                example.copyExecutionResults(c)
              }
            }
            this.afterSpecHasBeenExecuted = s.afterSpecHasBeenExecuted
            this.afterSpecFailure = s.afterSpecFailure
            executed = true
          }
        }
      }
    } catch { 
      case e: PathException => throw e
      case _ => ()
    }
    if (!executed)
      example.execution.map(_.execute)
    
    this
  }
  /** @return a clone of the specification */
  private[specification] def cloneSpecification = {
    tryToCreateObject[BaseSpecification with ExpectableFactory](getClass.getName, false, false)
  }
}
case class PathException(m: String) extends Exception(m)