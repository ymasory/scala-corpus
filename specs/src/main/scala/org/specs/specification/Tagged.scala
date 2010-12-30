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
import scala.collection.mutable.Queue

/**
 * The Tagged trait allows to add tags to a Specification or a System under test.<p/>
 * The tags will be propagated to the specification or the sus components using the tagWith method. With this rule, 
 * a Tagged component should always have its parent tags (and possibly more).<p/>
 * 
 * Tags can be used when executing a Specification to restrict the executed examples. In that case, the examples are marked as skipped:<pre>
 * object mySpec extends Specification
 * mySpec.accept("unit").reject("functional") // this means that only the examples tagged as "unit" and not "functional" will be executed. 
 * </pre>
 * 
 * Any Tagged object can be tagged using the "tag" method, accepting any number of strings:<pre>
 * "this example is tagged" in {
 *   // expect something
 * } tag("unit", "sample")
 * </pre>
 */
trait Tagged {
  /** list of all tags */
  private[specification] val tagList: Queue[Tag] = new Queue[Tag]
  /** list of tags which are accepted for this element */
  private[specification] val accepted: Queue[Tag] = new Queue[Tag]
  /** list of tags which are rejected for this element */
  private[specification] val rejected: Queue[Tag] = new Queue[Tag]

  /** transforms a string to a Tag object */
  implicit def stringToTag(s: String) = Tag(s) 

  /** @return the list of tags */
  def tagNames = tagList.toList map (_.name)
  /** Add one or several tags to this element */
  def tag(t: String*): this.type = addTags(t:_*)
  /** Clear all tags, accepted and rejected */
  def clearTags: this.type = { 
    tagList.clear
    accepted.clear
    rejected.clear
    taggedComponents.foreach(_.clearTags)
    this 
  }

  /** Add one tag to this element */
  def addTag(t: String): this.type = { 
    tagList.enqueue(Tag(t))
    propagateTagsToComponents
    this 
  }

  /** Add one or several tags to this element */
  def addTags(t: String*): this.type = { t.foreach(addTag(_)); this }
  
  /** 
   * Declare that this element should be accepted only if it has one of the accepted tags.
   *  This method declares the same thing for the components of this element.
   */
  def accept(t: Tag*): this.type = { 
    accepted.enqueue(t:_*)
    propagateTagsToComponents
    this
  }
  /** alias for the acceptTag method */
  def acceptTag(s: String*): this.type = acceptTags(s:_*) 
  /** alias for the accept method with strings */
  def acceptTags(s: String*): this.type = { s.foreach(accept(_)); this } 
  /** reset accepted and rejected to an empty queue */
  def acceptAnyTag: this.type = {
    accepted.clear
    rejected.clear
    taggedComponents.foreach(_.acceptAnyTag)
    this 
 }
  /** 
   * Declare that this element should be rejected if it has one of the rejected tags.
   *  This method declares the same thing for the components of this element.
   */
  def reject(t: Tag*): this.type = { 
    rejected.enqueue(t:_*)
    propagateTagsToComponents
    this 
  }
  /** alias for the reject method with strings */
  def rejectTag(s: String*): this.type = { s.foreach(reject(_)); this } 
  /** alias for the reject method with several strings */
  def rejectTags(s: String*): this.type = rejectTag(s:_*) 

  /** 
   * Return true if this Tagged element:<ul>
   * <li>doesn't have any tags and no tags are marked as accepted
   * <li>or doesn't have any accepted tags
   * <li>or has at least one of the accepted tags and doesn't have any of the rejected tags
   * </ul>
   * 
   * @return true if the element can be accepted, considering the tags it owns and the accepted/rejected tags
   */
  def isAccepted = {
    tagList.isEmpty && accepted.isEmpty ||
    (accepted.isEmpty ||
    !accepted.isEmpty && tagList.exists(t => accepted.exists(a => t matches a))) && 
    !tagList.exists(t => rejected.exists(a => t.matches(a)))
  }

  /** @return a description of the Tagged element showing the owned tags, the accepted and rejected tags */
  def tagSpec = "tags: " + tagList.mkString(", ") + "  accepted: " + accepted.mkString(", ") + "  rejected: " + rejected.mkString(", ")

  /** add the tags specification from another tagged element. This is used when propagating the tags from a specification to a sus for example */
  def tagWith(other: Tagged): this.type = {
    this.addTags(other.tagList.map(_.name):_*).
      acceptTags(other.accepted.map(_.name):_*).
      rejectTags(other.rejected.map(_.name):_*)
  }
  /** this method should be overriden if the Tagged element has Tagged components which should be tagged when this element is tagged */
  def taggedComponents: Seq[Tagged] = List()

  /** create another tagged with this new tags */
  def makeTagged(s: String*) = (new Tagged() {}).tagWith(this).tag(s:_*)

  /** add tags, accepted and rejected to the tagged components if there are some */
  private def propagateTagsToComponents = taggedComponents.foreach(_.tagWith(this))
}

/** This class encapsulate tag strings as an object. The tag name is interpreted as a regex */
case class Tag(name: String) {
  /** @return trueThis class encapsulate tag strings as an object. The tag name is interpreted as a regex */
  def matches(pattern: Tag) = {
    try {(name matches pattern.name) || (pattern.name matches name)}
    catch {
      case e: java.util.regex.PatternSyntaxException => false
      case e: NullPointerException => false
    }
  }
}
