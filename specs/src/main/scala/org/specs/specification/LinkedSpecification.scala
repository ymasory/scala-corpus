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
import org.specs._

/**
 * This trait adds the possibility to declare an included specification as "linked" in order to 
 * control its reporting in a separate file for example.
 */
trait LinkedSpecification { this: BaseSpecification => 
  /** storing the parent links for this specification */
  private var parentLinks = List[LinkedSpecification]()
  /** add a new parent link to this specification */
  private[specs] def addParent(s: LinkedSpecification): this.type = { parentLinks = s :: parentLinks; this }
  /** @return true if this specification has s as one of its parents */
  private[specs] def hasParent(s: LinkedSpecification): Boolean = parentLinks.contains(s)
  /** link this specification to a subordinate one */
  def linkTo(subSpec: Specification): this.type = {
    if (!contains(subSpec)) include(subSpec)
    subSpec.addParent(this)
    this
  }
  /** 
   * partitions the subspecifications in a pair where the first member is the list of linked specifications, 
   * and the second member is the unlinked ones
   */
  def partitionLinkedSpecifications: (List[Specification], List[Specification]) = {
    this.subSpecifications.partition(_.hasParent(this))  
  }
  /** @return the linked specifications */
  def linkedSpecifications = this.partitionLinkedSpecifications._1
  /** @return the unlinked specifications */
  def unlinkedSpecifications = this.partitionLinkedSpecifications._2 
}
